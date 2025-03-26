package dotty.tools.pc

import scala.collection.mutable
import scala.util.control.NonFatal

import scala.meta.pc.PcSymbolKind
import scala.meta.pc.PcSymbolProperty

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.Symbols.*
import dotty.tools.pc.utils.InteractiveEnrichments.deepDealias
import dotty.tools.pc.SemanticdbSymbols
import dotty.tools.pc.utils.InteractiveEnrichments.allSymbols
import dotty.tools.pc.utils.InteractiveEnrichments.stripBackticks
import scala.meta.internal.pc.PcSymbolInformation
import scala.meta.internal.pc.SymbolInfo
import dotty.tools.dotc.core.Denotations.{Denotation, MultiDenotation}
import scala.meta.pc.InspectResult
import org.eclipse.{lsp4j => l}
import scala.meta.internal.pc.InspectResultImpl
import scala.meta.internal.pc.InspectResultParamsListImp
import dotty.tools.pc.utils.InteractiveEnrichments.*

class SymbolInformationProvider(using Context):

  def info(symbol: String): Option[PcSymbolInformation] =
    val foundSymbols = SymbolProvider.compilerSymbols(symbol)

    val (searchedSymbol, alternativeSymbols) =
      foundSymbols.partition(compilerSymbol =>
        SemanticdbSymbols.symbolName(compilerSymbol) == symbol
      )

    searchedSymbol match
      case Nil => None
      case sym :: _ =>
        val classSym = if sym.isClass then sym else sym.moduleClass
        val parents =
          if classSym.isClass
          then classSym.asClass.parentSyms.map(SemanticdbSymbols.symbolName)
          else Nil
        val allParents =
          val visited = mutable.Set[Symbol]()
          def collect(sym: Symbol): Unit = {
            visited += sym
            if sym.isClass
            then sym.asClass.parentSyms.foreach {
              case parent if !visited(parent) =>
                  collect(parent)
              case _ =>
            }
          }
          collect(classSym)
          visited.toList.map(SemanticdbSymbols.symbolName)
        val dealisedSymbol =
          if sym.isAliasType then sym.info.deepDealias.typeSymbol else sym
        val classOwner =
          sym.ownersIterator.drop(1).find(s => s.isClass || s.is(Flags.Module))
        val overridden = sym.denot.allOverriddenSymbols.toList
        val memberDefAnnots =
          if classSym.exists then
            classSym.info
              .membersBasedOnFlags(Flags.Method, Flags.EmptyFlags)
              .flatMap(_.allSymbols)
              .flatMap(_.denot.annotations)
          else Nil

        val pcSymbolInformation =
          PcSymbolInformation(
            symbol = SemanticdbSymbols.symbolName(sym),
            kind = getSymbolKind(sym),
            parents = parents,
            dealiasedSymbol = SemanticdbSymbols.symbolName(dealisedSymbol),
            classOwner = classOwner.map(SemanticdbSymbols.symbolName),
            overriddenSymbols = overridden.map(SemanticdbSymbols.symbolName),
            alternativeSymbols =
              alternativeSymbols.map(SemanticdbSymbols.symbolName),
            properties =
              if sym.is(Flags.Abstract) then List(PcSymbolProperty.ABSTRACT)
              else Nil,
            recursiveParents = allParents,
            annotations = sym.denot.annotations.map(_.symbol.showFullName),
            memberDefsAnnotations = memberDefAnnots.map(_.symbol.showFullName).toList
          )

        Some(pcSymbolInformation)
    end match
  end info

  def inspect(fqcn: String, inspectLevel: Integer): List[InspectResult] = {
    val symbols =
      try SymbolProvider.toSymbols(SymbolInfo.getPartsFromFQCN(fqcn)).filterNot(_.is(Flags.Synthetic))
      catch case NonFatal(e) => Nil

    def resultWithMembers(symbol: Symbol, members: List[InspectResultImpl]) =
      InspectResultImpl(
        SemanticdbSymbols.symbolName(symbol),
        getSymbolLspKind(symbol),
        symbol.info.finalResultType.show,
        accessString(symbol),
        symbol.paramSymss.collect { case params =>
          val isType = params.headOption.map(_.is(Flags.TypeParam)).getOrElse(true)
          InspectResultParamsListImp(
            if (isType) params.map(_.decodedName)
            else params.map(p => s"${p.decodedName}: ${p.info.show}"),
            isType0 = isType,
            implicitOrUsingKeyword =
              if params.headOption.exists(_.is(Flags.Implicit)) then "implicit"
              else if params.headOption.exists(_.is(Flags.Given)) then "using"
              else ""
          )
        },
        members
      )

    symbols.map { symbol =>
      val members =
        if (inspectLevel > 0) symbol.info.allMembers.collect {
          case denot
              if !denot.symbol.is(Flags.Synthetic) && !(denot.symbol.is(Flags.Method) && SymbolProvider.ignoredMethodsForInspect(denot.symbol.decodedName)) =>
            resultWithMembers(denot.symbol, Nil)
        }.toList
        else Nil
      resultWithMembers(symbol, members)
    }
  }

  private def accessString(sym: Symbol): String =
    if (sym.privateWithin == NoSymbol)
      if (sym.isAllOf(Flags.PrivateLocal)) "private[this] "
      else if (sym.is(Flags.Private)) "private "
      else if (sym.isAllOf(Flags.ProtectedLocal)) "protected[this] "
      else if (sym.is(Flags.Protected)) "protected "
      else "public "
    else
      val ssym = sym.privateWithin.decodedName
      if (sym.is(Flags.Protected)) s"protected[$ssym] "
      else s"private[$ssym] "

  private def getSymbolKind(sym: Symbol): PcSymbolKind =
    if sym.isAllOf(Flags.JavaInterface) then PcSymbolKind.INTERFACE
    else if sym.is(Flags.Trait) then PcSymbolKind.TRAIT
    else if sym.isConstructor then PcSymbolKind.CONSTRUCTOR
    else if sym.isPackageObject then PcSymbolKind.PACKAGE_OBJECT
    else if sym.isClass then PcSymbolKind.CLASS
    else if sym.is(Flags.Macro) then PcSymbolKind.MACRO
    else if sym.is(Flags.Local) then PcSymbolKind.LOCAL
    else if sym.is(Flags.Method) then PcSymbolKind.METHOD
    else if sym.is(Flags.Param) then PcSymbolKind.PARAMETER
    else if sym.is(Flags.Package) then PcSymbolKind.PACKAGE
    else if sym.is(Flags.TypeParam) then PcSymbolKind.TYPE_PARAMETER
    else if sym.isType then PcSymbolKind.TYPE
    else PcSymbolKind.UNKNOWN_KIND

  private def getSymbolLspKind(sym: Symbol): l.SymbolKind =
    if sym.isAllOf(Flags.JavaInterface) then l.SymbolKind.Interface
    else if sym.is(Flags.Trait) then l.SymbolKind.Interface
    else if sym.isConstructor then l.SymbolKind.Constructor
    else if sym.is(Flags.Module) then l.SymbolKind.Module
    else if sym.isClass then l.SymbolKind.Class
    else if sym.is(Flags.Method) && sym.owner.is(Flags.ModuleClass) then l.SymbolKind.Function
    else if sym.is(Flags.Method) then l.SymbolKind.Method
    else if sym.is(Flags.Package) then l.SymbolKind.Package
    else if sym.isType then l.SymbolKind.Class
    else l.SymbolKind.Package
end SymbolInformationProvider

object SymbolProvider:

  val ignoredMethodsForInspect: Set[String] =
    Set(
      "synchronized", "##", "!=", "==", "ne", "eq", "finalize", "wait", "wait",
      "wait", "notifyAll", "notify", "toString", "clone", "equals", "hashCode",
      "getClass", "asInstanceOf", "isInstanceOf"
    )

  def compilerSymbol(symbol: String)(using Context): Option[Symbol] =
    compilerSymbols(symbol).find(sym => SemanticdbSymbols.symbolName(sym) == symbol)

  def compilerSymbols(symbol: String)(using Context): List[Symbol] =
    try toSymbols(SymbolInfo.getPartsFromSymbol(symbol))
    catch case NonFatal(e) => Nil

  private def normalizePackage(pkg: String): String =
    pkg.replace("/", ".").nn.stripSuffix(".")

  def toSymbols(info: SymbolInfo.SymbolParts)(using Context): List[Symbol] =
    def collectSymbols(denotation: Denotation): List[Symbol] =
      denotation match
        case MultiDenotation(denot1, denot2) =>
          collectSymbols(denot1) ++ collectSymbols(denot2)
        case denot => List(denot.symbol)

    def loop(
        owners: List[Symbol],
        parts: List[(String, Option[Boolean])],
    ): List[Symbol] =
      parts match
        case (head, isClass) :: tl =>
          val foundSymbols =
            owners.flatMap { owner =>
              val name = head.stripBackticks
              val next =
                isClass match {
                  case Some(true) => List(owner.info.member(typeName(name)))
                  case Some(false) => List(owner.info.member(termName(name)))
                  case None => List(owner.info.member(typeName(name)), owner.info.member(termName(name)))
                }
              next.flatMap(collectSymbols).filter(_.exists)
            }
          if foundSymbols.nonEmpty then loop(foundSymbols, tl)
          else Nil
        case Nil => owners

    val pkgSym =
      if info.packagePart == "_empty_/" then requiredPackage(nme.EMPTY_PACKAGE)
      else requiredPackage(normalizePackage(info.packagePart))
    val found = loop(List(pkgSym), info.names)
    info.paramName match
      case Some(name) => found.flatMap(_.paramSymss.flatten.find(_.showName == name))
      case _ => found
  end toSymbols
