package pl.ekodo.json

import org.scalatest.{FlatSpec, Matchers}

class JsonToScalaSpec extends FlatSpec with Matchers with JsonToScalaFixtures {

  "JsonToScala" should "parse empty json object" in {
    val result = JsonToScala("Empty", json1)
    result.name shouldEqual "Empty"
    result.fields shouldBe empty
  }

  it should "parse json with one field" in {
    val result = JsonToScala("A", json2)
    result.name shouldEqual "A"
    result.fields("a") shouldBe StringType
  }

  it should "parse json with two fields" in {
    val result = JsonToScala("A", json3)
    result.name shouldEqual "A"
    result.fields("a") shouldBe StringType
    result.fields("b") shouldBe StringType
  }

  it should "parse json with nested object" in {
    val result = JsonToScala("A", json4)
    result.name shouldEqual "A"
    result.fields("c") shouldBe CaseClass("C", Map("c" -> BigDecimalType))
  }

}

trait JsonToScalaFixtures {

  val json1 =
    """
      |{}
    """.stripMargin

  val json2 =
    """
      |{
      |"a" : "a"
      |}
    """.stripMargin

  val json3 =
    """
      |{
      |"a" : "a",
      |"b" : "b"
      |}
    """.stripMargin

  val json4 =
    """
      |{
      |"a" : "a",
      |"b" : "b",
      |"c" : {
      |  "c" : 1
      |}
      |}
    """.stripMargin
}
