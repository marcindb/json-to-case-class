package pl.ekodo.json.transformation

import org.scalatest.{FlatSpec, Matchers}
import pl.ekodo.json.model._

class JsonUnmarschallerSpec extends FlatSpec with Matchers with JsonUnmarschallerFixtures {

  "JsonToScala" should "parse empty json object" in {
    val result = JsonUnmarschaller("Empty", json1)
    result.name shouldEqual "Empty"
    result.fields shouldBe empty
  }

  it should "parse json with one field" in {
    val result = JsonUnmarschaller("A", json2)
    result.name shouldEqual "A"
    result.fields("a") shouldBe StringType
  }

  it should "parse json with two fields" in {
    val result = JsonUnmarschaller("A", json3)
    result.name shouldEqual "A"
    result.fields("a") shouldBe StringType
    result.fields("b") shouldBe StringType
  }

  it should "parse json with nested object" in {
    val result = JsonUnmarschaller("A", json4)
    result.name shouldEqual "A"
    result.fields("c") shouldBe CaseClass("C", Map("c" -> IntType), List("A"))
  }

  it should "parse json with empty array" in {
    val result = JsonUnmarschaller("A", json5)
    result.name shouldEqual "A"
    result.fields("a") shouldBe StringType
    result.fields("b") shouldBe SeqType(AnyType)
  }

}

trait JsonUnmarschallerFixtures {

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

  val json5 =
    """
      |{
      |"a" : "a",
      |"b" : []
      |}
    """.stripMargin
}
