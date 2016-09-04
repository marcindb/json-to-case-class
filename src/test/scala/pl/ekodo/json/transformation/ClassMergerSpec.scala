package pl.ekodo.json.transformation

import org.scalatest.{FlatSpec, Matchers}
import pl.ekodo.json.model._

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

  it should "replace all occurrences of merged classes with proper type" in {
    val fields = Map("a" -> StringType, "b" -> IntType)
    val classA = CaseClass("A", fields)
    val classB = CaseClass("B", fields)
    val classC = CaseClass("C", Map("a" -> classA, "b" -> classB))
    val merged = ClassMerger(List(classA, classB, classC))

    val expectedC = CaseClass("C", Map("a" -> classA, "b" -> classA))
    merged should contain only(classA.copy(replace = Set(classB)), expectedC)
  }

  it should "replace all occurrences of merged classes with proper type for optional types" in {
    val fields = Map("a" -> StringType, "b" -> IntType)
    val classA = CaseClass("A", fields)
    val classB = CaseClass("B", fields)
    val classC = CaseClass("C", Map("a" -> classA, "b" -> OptionalType(classB)))
    val merged = ClassMerger(List(classA, classB, classC))

    val expectedC = CaseClass("C", Map("a" -> classA, "b" -> OptionalType(classA)))
    merged should contain only(classA.copy(replace = Set(classB)), expectedC)
  }

  it should "replace all occurrences of merged classes with proper type for sequence types" in {
    val fields = Map("a" -> StringType, "b" -> IntType)
    val classA = CaseClass("A", fields)
    val classB = CaseClass("B", fields)
    val classC = CaseClass("C", Map("a" -> classA, "b" -> SeqType(classB)))
    val merged = ClassMerger(List(classA, classB, classC))

    val expectedC = CaseClass("C", Map("a" -> classA, "b" -> SeqType(classA)))
    merged should contain only(classA.copy(replace = Set(classB)), expectedC)
  }

}
