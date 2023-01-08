package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*
import Symbols.*
import Types.*
import StdNames.*
import NameKinds.OuterSelectName
import NameKinds.SuperAccessorName

import ast.tpd.*
import config.Printers.init as printer
import reporting.StoreReporter
import reporting.trace as log

import Errors.*

import Semantic.{ NewExpr, Call, TraceValue, Trace, PolyFun, Arg, ByNameArg }

import Semantic.{ typeRefOf, hasSource, extendTrace, withTrace, trace }

import scala.collection.mutable
import scala.annotation.tailrec

/** Check initialization safety of static objects
 *
 *  The problem is illustrated by the example below:
 *
 *      class Foo(val opposite: Foo)
 *      case object A extends Foo(B)     // A -> B
 *      case object B extends Foo(A)     // B -> A
 *
 *  In the code above, the initialization of object `A` depends on `B` and vice
 *  versa. There is no correct way to initialize the code above. The current
 *  checker issues a warning for the code above.
 *
 *  At the high-level, the analysis has the following characteristics:
 *
 *  1. It is inter-procedural and flow-insensitive.
 *
 *  2. It is receiver-sensitive but not heap-sensitive nor parameter-sensitive.
 *
 *     Fields and parameters are always abstracted by their types.
 *
 */
object Objects:

  // ----------------------------- abstract domain -----------------------------

  sealed abstract class Value:
    def show(using Context): String


  /**
   * A reference caches the current value.
   */
  sealed abstract class Ref extends Value:
    private val fields: mutable.Map[Symbol, Value] = mutable.Map.empty
    private val outers: mutable.Map[ClassSymbol, Value] = mutable.Map.empty

    def fieldValue(sym: Symbol): Value = fields(sym)

    def outerValue(cls: ClassSymbol): Value = outers(cls)

    def hasOuter(cls: ClassSymbol): Boolean = outers.contains(cls)

    def updateField(field: Symbol, value: Value) =
      assert(!fields.contains(field), "Field already set " + field)
      fields(field) = value

    def updateOuter(cls: ClassSymbol, value: Value) =
      assert(!outers.contains(cls), "Outer already set " + cls)
      outers(cls) = value

  /** A reference to a static object */
  case class ObjectRef(klass: ClassSymbol) extends Ref:
    def show(using Context) = "ObjectRef(" + klass.show + ")"

  /**
   * Rerepsents values that are instances of the specified class
   *
   * `tp.classSymbol` should be the concrete class of the value at runtime.
   */
  case class OfClass(tp: Type, outer: Value) extends Ref:
    def show(using Context) = "OfClass(" + tp.show + ")"

  /**
   * Rerepsents values of a specific type
   *
   * `OfType` is just a short-cut referring to currently instantiated sub-types.
   *
   * Note: this value should never be an index in the cache.
   */
  case class OfType(tp: Type) extends Value:
    def show(using Context) = "OfType(" + tp.show + ")"

  /**
   * Represents a lambda expression
   */
  case class Fun(expr: Tree, thisV: Ref, klass: ClassSymbol) extends Value:
    def show(using Context) = "Fun(" + expr.show + ", " + thisV.show + ", " + klass.show + ")"

  /**
   * Represents a set of values
   *
   * It comes from `if` expressions.
   */
  case class RefSet(refs: List[Value]) extends Value:
    assert(refs.forall(!_.isInstanceOf[RefSet]))
    def show(using Context) = refs.map(_.show).mkString("[", ",", "]")

  val Bottom = RefSet(Nil)

  object State:
    /**
     * Remembers the instantiated types during instantiation of a static object.
     */
    class Data:
      // objects under check
      private[State] val checkingObjects = new mutable.ArrayBuffer[ClassSymbol]
      private[State] val checkedObjects = new mutable.ArrayBuffer[ClassSymbol]

      // object -> (class, types of the class)
      private[State] val instantiatedTypes = mutable.Map.empty[ClassSymbol, Map[ClassSymbol, List[Type]]]

      // object -> (fun type, fun values)
      private[State] val instantiatedFuncs = mutable.Map.empty[ClassSymbol, Map[Type, List[Fun]]]

    def checkCycle(clazz: ClassSymbol)(work: => Unit)(using data: Data, ctx: Context) =
      val index = data.checkingObjects.indexOf(clazz)

      if index != -1 then
        val cycle = data.checkingObjects.slice(index, data.checkingObjects.size - 1)
        report.warning("Cyclic initialization: " + cycle.map(_.show).mkString(" -> ") + clazz.show, clazz.defTree)
      else if data.checkedObjects.indexOf(clazz) == -1 then
        data.checkingObjects += clazz
        work
        assert(data.checkingObjects.last == clazz, "Expect = " + clazz.show + ", found = " + data.checkingObjects.last)
        data.checkedObjects += data.checkingObjects.remove(data.checkingObjects.size - 1)

  type Contextual[T] = (Context, State.Data, Cache.Data, Trace) ?=> T

  object Cache:
    /** Cache for method calls and lazy values
     *
     *  Symbol -> ThisValue -> ReturnValue
     */
    private type Cache = Map[Symbol, Map[Value, Value]]

    class Data:
      /** The cache from last iteration */
      private var last: Cache =  Map.empty

      /** The output cache
       *
       *  The output cache is computed based on the cache values `last` from the
       *  last iteration.
       *
       *  Both `last` and `current` are required to make sure evaluation happens
       *  once in each iteration.
       */
      private var current: Cache = Map.empty

      /** Whether the current heap is different from the last heap?
       *
       *  `changed == false` implies that the fixed point has been reached.
       */
      private var changed: Boolean = false

      def assume(value: Value, sym: Symbol)(fun: => Value): Contextual[Value] =
        val assumeValue: Value =
          last.get(value, sym) match
          case Some(value) => value
          case None =>
            val default = Bottom
            this.last = last.updatedNested(value, sym, default)
            default

        this.current = current.updatedNested(value, sym, assumeValue)

        val actual = fun
        if actual != assumeValue then
          this.changed = true
          this.current = this.current.updatedNested(value, sym, actual)
        end if

        actual
      end assume

      def hasChanged = changed

      /** Prepare cache for the next iteration
       *
       *  1. Reset changed flag.
       *
       *  2. Use current cache as last cache and set current cache to be empty.
       *
       */
      def prepareForNextIteration()(using Context) =
        this.changed = false
        this.last = this.current
        this.current = Map.empty
    end Data

    extension (cache: Cache)
      private def get(value: Value, sym: Symbol): Option[Value] =
        cache.get(sym).flatMap(_.get(value))

      private def removed(value: Value, sym: Symbol) =
        val innerMap2 = cache(sym).removed(value)
        cache.updated(sym, innerMap2)

      private def updatedNested(value: Value, sym: Symbol, result: Value): Cache =
        val innerMap = cache.getOrElse(sym, Map.empty[Value, Value])
        val innerMap2 = innerMap.updated(value, result)
        cache.updated(sym, innerMap2)
    end extension
  end Cache

  // --------------------------- domain operations -----------------------------

  type ArgInfo = TraceValue[Value]

  extension (a: Value)
    def join(b: Value): Value =
      (a, b) match
      case (Bottom, b)                        => b
      case (a, Bottom)                        => a
      case (RefSet(refs1), RefSet(refs2))     => RefSet(refs1 ++ refs2)
      case (a, RefSet(refs))                  => RefSet(a :: refs)
      case (RefSet(refs), b)                  => RefSet(b :: refs)
      case (a, b)                             => RefSet(a :: b :: Nil)

    def widen(using Context): Value = a.match
      case RefSet(refs) => refs.map(_.widen).join
      case OfClass(tp, _: OfClass) => OfType(tp)
      case _ => a

  extension (values: Seq[Value])
    def join: Value = values.reduce { (v1, v2) => v1.join(v2) }

  def call(thisV: Value, meth: Symbol, args: List[ArgInfo], receiver: Type, superType: Type, needResolve: Boolean = true): Contextual[Value] =
    thisV match

    case ObjectRef(klass) =>
      ???

    case OfClass(tp, outer) =>
      ???

    case OfType(tp) =>
      ???

    case Fun(expr, thisV, klass) =>
      ???

    case RefSet(vs) =>
      ???

  def callConstructor(thisV: Value, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = ???

  def select(thisV: Value, field: Symbol, receiver: Type, needResolve: Boolean = true): Contextual[Value] = ???

  def instantiate(outer: Value, klass: ClassSymbol, ctor: Symbol, args: List[ArgInfo]): Contextual[Value] = ???

  // -------------------------------- algorithm --------------------------------

  /** Check an individual object */
  private def accessObject(classSym: ClassSymbol)(using Context, State.Data): Value =
    val tpl = classSym.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]

    @tailrec
    def iterate()(using Context): Unit =
      given cache: Cache.Data = new Cache.Data
      given Trace = Trace.empty

      init(tpl, ObjectRef(classSym), classSym)

      val hasError = ctx.reporter.pendingMessages.nonEmpty
      if cache.hasChanged && !hasError then
        cache.prepareForNextIteration()
        iterate()
      else
        ctx.reporter.flush()
    end iterate

    State.checkCycle(classSym) {
      val reporter = new StoreReporter(ctx.reporter)
      iterate()(using ctx.fresh.setReporter(reporter))
    }

    ObjectRef(classSym)

  def checkClasses(classes: List[ClassSymbol])(using Context): Unit =
    given State.Data = new State.Data

    for
      classSym <- classes  if classSym.isStaticObject
    do
      accessObject(classSym)


  /** Evaluate a list of expressions */
  def evalExprs(exprs: List[Tree], thisV: Ref, klass: ClassSymbol): Contextual[List[Value]] =
    exprs.map { expr => eval(expr, thisV, klass) }

  /** Handles the evaluation of different expressions
   *
   * @param expr   The expression to be evaluated.
   * @param thisV  The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass  The enclosing class where the expression `expr` is located.
   */
  def eval(expr: Tree, thisV: Ref, klass: ClassSymbol): Contextual[Value] =
    expr match
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Bottom

      case id @ Ident(name) if !id.symbol.is(Flags.Method)  =>
        assert(name.isTermName, "type trees should not reach here")
        evalType(expr.tpe, thisV, klass)

      case NewExpr(tref, New(tpt), ctor, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        val cls = tref.classSymbol.asClass
        val outer = outerValue(tref, thisV, klass)
        instantiate(outer, cls, ctor, args)

      case Call(ref, argss) =>
        // check args
        val args = evalArgs(argss.flatten, thisV, klass)

        ref match
        case Select(supert: Super, _) =>
          val SuperType(thisTp, superTp) = supert.tpe: @unchecked
          val thisValue2 = extendTrace(ref) { resolveThis(thisTp.classSymbol.asClass, thisV, klass) }
          call(thisValue2, ref.symbol, args, thisTp, superTp)

        case Select(qual, _) =>
          val receiver = eval(qual, thisV, klass)
          if ref.symbol.isConstructor then
            callConstructor(receiver, ref.symbol, args)
          else
            call(receiver, ref.symbol, args, receiver = qual.tpe, superType = NoType)

        case id: Ident =>
          id.tpe match
          case TermRef(NoPrefix, _) =>
            // resolve this for the local method
            val enclosingClass = id.symbol.owner.enclosingClass.asClass
            val thisValue2 = extendTrace(ref) { resolveThis(enclosingClass, thisV, klass) }
            // local methods are not a member, but we can reuse the method `call`
            call(thisValue2, id.symbol, args, receiver = NoType, superType = NoType, needResolve = false)
          case TermRef(prefix, _) =>
            val receiver = evalType(prefix, thisV, klass)
            if id.symbol.isConstructor then
              callConstructor(receiver, id.symbol, args)
            else
              call(receiver, id.symbol, args, receiver = prefix, superType = NoType)

      case Select(qualifier, name) =>
        val qual = eval(qualifier, thisV, klass)

        name match
          case OuterSelectName(_, _) =>
            val current = qualifier.tpe.classSymbol
            val target = expr.tpe.widenSingleton.classSymbol.asClass
            resolveThis(target, qual, current.asClass)
          case _ =>
            select(qual, expr.symbol, receiver = qualifier.tpe)

      case _: This =>
        evalType(expr.tpe, thisV, klass)

      case Literal(_) =>
        Bottom

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot))
          Bottom
        else
          eval(expr, thisV, klass)

      case NamedArg(name, arg) =>
        eval(arg, thisV, klass)

      case Assign(lhs, rhs) =>
        lhs match
        case Select(qual, _) =>
          eval(qual, thisV, klass)
          eval(rhs, thisV, klass)
        case id: Ident =>
          eval(rhs, thisV, klass)

      case closureDef(ddef) =>
        Fun(ddef.rhs, thisV, klass)

      case PolyFun(body) =>
        Fun(body, thisV, klass)

      case Block(stats, expr) =>
        evalExprs(stats, thisV, klass)
        eval(expr, thisV, klass)

      case If(cond, thenp, elsep) =>
        evalExprs(cond :: thenp :: elsep :: Nil, thisV, klass).join

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Bottom
        else eval(arg, thisV, klass)

      case Match(selector, cases) =>
        eval(selector, thisV, klass)
        evalExprs(cases.map(_.body), thisV, klass).join

      case Return(expr, from) =>
        eval(expr, thisV, klass)

      case WhileDo(cond, body) =>
        evalExprs(cond :: body :: Nil, thisV, klass)
        Bottom

      case Labeled(_, expr) =>
        eval(expr, thisV, klass)

      case Try(block, cases, finalizer) =>
        eval(block, thisV, klass)
        if !finalizer.isEmpty then
          eval(finalizer, thisV, klass)
        evalExprs(cases.map(_.body), thisV, klass).join

      case SeqLiteral(elems, elemtpt) =>
        evalExprs(elems, thisV, klass).join

      case Inlined(call, bindings, expansion) =>
        evalExprs(bindings, thisV, klass)
        eval(expansion, thisV, klass)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Bottom

      case vdef : ValDef =>
        // local val definition
        eval(vdef.rhs, thisV, klass)

      case ddef : DefDef =>
        // local method
        Bottom

      case tdef: TypeDef =>
        // local type definition
        Bottom

      case _: Import | _: Export =>
        Bottom

      case _ =>
        report.error("[Internal error] unexpected tree" + Trace.show, expr)
        OfType(expr.tpe)

  /** Handle semantics of leaf nodes
   *
   * For leaf nodes, their semantics is determined by their types.
   *
   * @param tp      The type to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by `klass`.
   * @param klass   The enclosing class where the type `tp` is located.
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elission happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def evalType(tp: Type, thisV: Ref, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] = log("evaluating " + tp.show, printer, (_: Value).show) {
    // TODO: identify aliasing of object & object field
    tp match
      case _: ConstantType =>
        Bottom

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        // - params and var definitions are abstract by its type
        // - evaluate the rhs of the local definition for val definitions
        val sym = tmref.symbol
        if sym.isOneOf(Flags.Param | Flags.Mutable) then
          OfType(sym.info)
        else if sym.is(Flags.Package) then
          Bottom
        else if sym.hasSource then
          val rhs = sym.defTree.asInstanceOf[ValDef].rhs
          eval(rhs, thisV, klass)
        else
          // pattern-bound variables
          OfType(sym.info)

      case tmref: TermRef =>
        val sym = tmref.symbol
        if sym.isStaticObject then
          // TODO: check immutability
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)
        else
          // TODO: check object field access
          val value = evalType(tmref.prefix, thisV, klass)
          select(value, tmref.symbol, tmref.prefix)

      case tp @ ThisType(tref) =>
        val sym = tref.symbol
        if sym.is(Flags.Package) then
          Bottom
        else if sym.isStaticObject && sym != klass then
          // TODO: check immutability
          if elideObjectAccess then
            ObjectRef(sym.moduleClass.asClass)
          else
            accessObject(sym.moduleClass.asClass)

        else
          resolveThis(tref.classSymbol.asClass, thisV, klass)

      case _ =>
        throw new Exception("unexpected type: " + tp)
  }

  /** Evaluate arguments of methods */
  def evalArgs(args: List[Arg], thisV: Ref, klass: ClassSymbol): Contextual[List[ArgInfo]] =
    val argInfos = new mutable.ArrayBuffer[ArgInfo]
    args.foreach { arg =>
      val res =
        if arg.isByName then
          Fun(arg.tree, thisV, klass)
        else
          eval(arg.tree, thisV, klass)

      argInfos += TraceValue(res, trace.add(arg.tree))
    }
    argInfos.toList

  def init(tpl: Template, thisV: Ref, klass: ClassSymbol): Contextual[Unit] =
    val paramsMap = tpl.constr.termParamss.flatten.map { vdef =>
      vdef.name -> thisV.fieldValue(vdef.symbol)
    }.toMap

    // init param fields
    klass.paramGetters.foreach { acc =>
      val value = paramsMap(acc.name.toTermName)
      thisV.updateField(acc, value)
      printer.println(acc.show + " initialized with " + value)
    }

    // Tasks is used to schedule super constructor calls.
    // Super constructor calls are delayed until all outers are set.
    type Tasks = mutable.ArrayBuffer[() => Unit]
    def superCall(tref: TypeRef, ctor: Symbol, args: List[ArgInfo], tasks: Tasks): Unit =
      val cls = tref.classSymbol.asClass
      // update outer for super class
      val res = outerValue(tref, thisV, klass)
      thisV.updateOuter(cls, res)

      // follow constructor
      if cls.hasSource then
        tasks.append { () =>
          printer.println("init super class " + cls.show)
          callConstructor(thisV, ctor, args)
          ()
        }

    // parents
    def initParent(parent: Tree, tasks: Tasks) =
      parent match
      case tree @ Block(stats, NewExpr(tref, New(tpt), ctor, argss)) =>  // can happen
        evalExprs(stats, thisV, klass)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tasks)

      case tree @ NewExpr(tref, New(tpt), ctor, argss) =>       // extends A(args)
        val args = evalArgs(argss.flatten, thisV, klass)
        superCall(tref, ctor, args, tasks)

      case _ =>   // extends A or extends A[T]
        val tref = typeRefOf(parent.tpe)
        superCall(tref, tref.classSymbol.primaryConstructor, Nil, tasks)

    // see spec 5.1 about "Template Evaluation".
    // https://www.scala-lang.org/files/archive/spec/2.13/05-classes-and-objects.html
    if !klass.is(Flags.Trait) then
      // outers are set first
      val tasks = new mutable.ArrayBuffer[() => Unit]

      // 1. first init parent class recursively
      // 2. initialize traits according to linearization order
      val superParent = tpl.parents.head
      val superCls = superParent.tpe.classSymbol.asClass
      extendTrace(superParent) { initParent(superParent, tasks) }

      val parents = tpl.parents.tail
      val mixins = klass.baseClasses.tail.takeWhile(_ != superCls)

      // The interesting case is the outers for traits.  The compiler
      // synthesizes proxy accessors for the outers in the class that extends
      // the trait. As those outers must be stable values, they are initialized
      // immediately following class parameters and before super constructor
      // calls and user code in the class body.
      mixins.reverse.foreach { mixin =>
        parents.find(_.tpe.classSymbol == mixin) match
        case Some(parent) =>
          extendTrace(parent) { initParent(parent, tasks) }
        case None =>
          // According to the language spec, if the mixin trait requires
          // arguments, then the class must provide arguments to it explicitly
          // in the parent list. That means we will encounter it in the Some
          // branch.
          //
          // When a trait A extends a parameterized trait B, it cannot provide
          // term arguments to B. That can only be done in a concrete class.
          val tref = typeRefOf(klass.typeRef.baseType(mixin).typeConstructor)
          val ctor = tref.classSymbol.primaryConstructor
          if ctor.exists then
            // The parameter check of traits comes late in the mixin phase.
            // To avoid crash we supply hot values for erroneous parent calls.
            // See tests/neg/i16438.scala.
            val args: List[ArgInfo] = ctor.info.paramInfoss.flatten.map(_ => new ArgInfo(Bottom, Trace.empty))
            extendTrace(superParent) {
              superCall(tref, ctor, args, tasks)
            }
      }

      // initialize super classes after outers are set
      tasks.foreach(task => task())
    end if

    // class body
    tpl.body.foreach {
      case vdef : ValDef if !vdef.symbol.is(Flags.Lazy) && !vdef.rhs.isEmpty =>
        val res = eval(vdef.rhs, thisV, klass)
        thisV.updateField(vdef.symbol, res)

      case _: MemberDef =>

      case tree =>
        eval(tree, thisV, klass)
    }


  /** Resolve C.this that appear in `klass`
   *
   * @param target  The class symbol for `C` for which `C.this` is to be resolved.
   * @param thisV   The value for `D.this` where `D` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `C.this` is located.
   * @param elideObjectAccess Whether object access should be omitted.
   *
   * Object access elission happens when the object access is used as a prefix
   * in `new o.C` and `C` does not need an outer.
   */
  def resolveThis(target: ClassSymbol, thisV: Value, klass: ClassSymbol, elideObjectAccess: Boolean = false): Contextual[Value] =
    if target == klass then
      thisV
    else if target.is(Flags.Package) then
      Bottom
    else if target.isStaticObject then
      val res = ObjectRef(target.moduleClass.asClass)
      if target == klass || elideObjectAccess then res
      else accessObject(target)
    else
      thisV match
        case Bottom => Bottom
        case ref: Ref =>
          val outerCls = klass.owner.lexicallyEnclosingClass.asClass
          if !ref.hasOuter(klass) then
            val error = "[Internal error] outer not yet initialized, target = " + target + ", klass = " + klass + Trace.show
            report.error(error, Trace.position)
            Bottom
          else
            resolveThis(target, ref.outerValue(klass), outerCls)
        case RefSet(refs) =>
          refs.map(ref => resolveThis(target, ref, klass)).join
        case fun: Fun =>
          report.error("[Internal error] unexpected thisV = " + thisV + ", target = " + target.show + ", klass = " + klass.show + Trace.show, Trace.position)
          Bottom
        case OfType(tp) =>
          OfType(target.appliedRef)

  /** Compute the outer value that correspond to `tref.prefix`
   *
   * @param tref    The type whose prefix is to be evaluated.
   * @param thisV   The value for `C.this` where `C` is represented by the parameter `klass`.
   * @param klass   The enclosing class where the type `tref` is located.
   */
  def outerValue(tref: TypeRef, thisV: Ref, klass: ClassSymbol): Contextual[Value] =
    val cls = tref.classSymbol.asClass
    if tref.prefix == NoPrefix then
      val enclosing = cls.owner.lexicallyEnclosingClass.asClass
      resolveThis(enclosing, thisV, klass, elideObjectAccess = cls.isStatic)
    else
      if cls.isAllOf(Flags.JavaInterface) then Bottom
      else evalType(tref.prefix, thisV, klass, elideObjectAccess = cls.isStatic)

  extension (sym: Symbol)
    def isStaticObject(using Context) =
      sym.is(Flags.Module, butNot = Flags.Package) && sym.isStatic
