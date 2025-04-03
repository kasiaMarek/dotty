package dotty.tools.pc.tests.info

import dotty.tools.pc.base.BasePCSuite
import scala.meta.internal.jdk.CollectionConverters._
import org.junit.Test
import scala.meta.pc.InspectResult
import scala.meta.pc.InspectResultParamsList
import org.eclipse.lsp4j.SymbolKind

class InspectSuite extends BasePCSuite {
  private def inspect(symbol: String) =
    presentationCompiler.inspect(symbol, 1).get().asScala.map(_.show).mkString("\n")

  @Test
  def `basic` =
    withSource(
      """|package a
         |class Foo
         |case class A(a: Int):
         |  def m: Foo = ???
         |  def k[T]: T = ???
         |object A:
         |  val g = 1
         |  object O
         |""".stripMargin)
    val info = inspect("a.A")
    assertNoDiff(
      """|public  Class A
         |	- public  Method k[T]: T
         |	- public  Method m: a.Foo
         |	- public  Variable a
         |public  Module A
         |	- public  Module O
         |	- public  Variable g
         |""".stripMargin,
         info
    )

  extension (info: InspectResult)
    def show: String =
      info.visibility().pad + info.kind().toString().pad + info.name +
      info.paramss().asScala.map(_.show).mkString("") +
      (if info.kind() == SymbolKind.Method then ": " + info.resultType() else "") +
      (if !info.members().isEmpty() then info.members().asScala.map(_.show).sorted.mkString("\n\t- ","\n\t- ", "") else "")
    def name: String =
      info.symbol().stripSuffix("().").split("[#\\.\\/]").last

  extension (info: InspectResultParamsList)
    def show: String =
      (if info.isType() then "[" else "(") +
      info.implicitOrUsingKeyword().pad +
      info.params().asScala.mkString(", ") +
      (if info.isType() then "]" else ")")

  extension (str: String)
    def pad = if str.isEmpty then "" else s"$str "
}