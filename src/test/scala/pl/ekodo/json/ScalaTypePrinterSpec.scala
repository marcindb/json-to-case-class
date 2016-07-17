package pl.ekodo.json

import org.scalatest.{FlatSpec, Matchers}

class ScalaTypePrinterSpec extends FlatSpec with Matchers {

  "ScalaTypePrinter" should "return string with class representation (class with one field)" in {
    val classA = CaseClass("Test", Map("a" -> StringType))
    val classAString =
      """
        |case class Test(
        |  a: String
        |)
      """.stripMargin
    ScalaTypePrinter(classA).trim shouldEqual classAString.trim
  }

  it should "return string with class representation (class with two fields)" in {
    val classA = CaseClass("Test", Map("a" -> StringType, "b" -> IntType))
    val classAString =
      """
        |case class Test(
        |  a: String,
        |  b: Int
        |)
      """.stripMargin
    ScalaTypePrinter(classA).trim shouldEqual classAString.trim
  }

}
