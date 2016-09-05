package pl.ekodo.json

import org.scalatest.{FlatSpec, Matchers}
import pl.ekodo.json.model.{CaseClass, OptionalType, SeqType, StringType}

class JsonToScalaSpec extends FlatSpec with Matchers {

  "JsonToScala" should "covert json line to case class" in {
    val json = List(
      """{"a": "a", "b" : "b", "c" : "c"}"""
    )
    val expectedClass = CaseClass("Root", Map("a" -> StringType, "b" -> StringType, "c" -> StringType))
    JsonToScala.convert(json, "Root") should contain only(expectedClass)
  }

  it should "covert json lines to case classes (1)" in {
    val json = List(
      """{"a": "a", "b" : "b", "c" : "c"}""",
      """{"a": "a", "b" : "b", "c" : "c"}"""
    )
    val expectedClass = CaseClass("Root", Map("a" -> StringType, "b" -> StringType, "c" -> StringType))
    JsonToScala.convert(json, "Root") should contain only (expectedClass)
  }

  it should "covert json lines to case classes (2)" in {
    val json = List(
      """{"a": "a", "b" : "b", "c" : "c"}""",
      """{"a": "a", "b" : "b"}"""
    )
    val expectedClass = CaseClass("Root", Map("a" -> StringType, "b" -> StringType, "c" -> OptionalType(StringType)))
    JsonToScala.convert(json, "Root") should contain only (expectedClass)

  }

  it should "covert json lines to case classes (3)" in {
    val json = List(
      """{"a": "a", "b" : {"b": "b"}, "c" : [{"c": "c"}]}""",
      """{"a": "a", "b" : {"b": "b"}}"""
    )
    val expectedBClass = CaseClass("B", Map("b" -> StringType), List("Root"))
    val expectedCClass = CaseClass("C", Map("c" -> StringType), List("Root"))
    val expectedRootClass = CaseClass(
      "Root",
      Map("a" -> StringType, "b" -> expectedBClass, "c" -> OptionalType(SeqType(expectedCClass))))

    JsonToScala.convert(json, "Root") should contain only (expectedRootClass, expectedBClass, expectedCClass)
  }

  it should "covert json lines to case classes (4)" in {
    val json = List(
      """{"a": "a", "b" : {"b": "b"}, "c" : [{"c": "c"}, {"c": "c", "d": "d"}]}""",
      """{"a": "a", "b" : {"b": "b"}}"""
    )
    val expectedBClass = CaseClass("B", Map("b" -> StringType), List("Root"))
    val expectedCClass = CaseClass("C", Map("c" -> StringType), List("Root"))
    val expectedRootClass = CaseClass(
      "Root",
      Map("a" -> StringType, "b" -> expectedBClass, "c" -> OptionalType(SeqType(expectedCClass))))

    JsonToScala.convert(json, "Root") should contain only(expectedRootClass, expectedBClass, expectedCClass)

  }

  it should "covert json lines to case classes (5)" in {

  }

}
