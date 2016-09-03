package pl.ekodo.json.transformation

import org.scalatest.{FlatSpec, Matchers}
import pl.ekodo.json.model.{CaseClass, IntType, StringType}

class ClassMergerSpec extends FlatSpec with Matchers {

  "ClassMerger" should "merge classes with the same fields" in {
    val fields = Map("a" -> StringType, "b" -> IntType)
    val classA = CaseClass("A", fields)
    val classB = CaseClass("B", fields)
    val merged = ClassMerger(List(classA, classB))

    merged.size shouldEqual 1
  }

  it should "retain fields from merged classes" in {
    val fields = Map("a" -> StringType, "b" -> IntType)
    val classA = CaseClass("A", fields)
    val classB = CaseClass("B", fields)
    val merged = ClassMerger(List(classA, classB))

    merged.head.fields shouldEqual fields
  }

  it should "not merge classes with different fields" in {
    val classA = CaseClass("A", Map("a" -> StringType, "b" -> IntType))
    val classB = CaseClass("B", Map("a" -> StringType, "c" -> IntType))
    val merged = ClassMerger(List(classA, classB))

    merged should contain only(classA, classB)
  }

}
