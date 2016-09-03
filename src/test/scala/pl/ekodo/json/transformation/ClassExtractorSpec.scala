package pl.ekodo.json.transformation

import org.scalatest.{FlatSpec, Matchers}
import pl.ekodo.json.model.{CaseClass, IntType, OptionalType, StringType}

class ClassExtractorSpec extends FlatSpec with Matchers {

  "CaseClass inference" should "return the same class if only one class is passed" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    ClassExtractor.infer(testClass) shouldEqual testClass
  }

  it should "return the same class if all passed classes have the same structure" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    ClassExtractor.infer(testClass, testClass, testClass, testClass) shouldEqual testClass
  }

  it should "throw an exception if passed classes have different names" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    val otherClass = CaseClass("other", Map("a" -> StringType, "b" -> IntType))
    intercept[IllegalArgumentException] {
      ClassExtractor.infer(testClass, otherClass) shouldEqual testClass
    }
  }

  it should "collect fields from all classes" in {
    val classA = CaseClass("test", Map("a" -> StringType))
    val classB = CaseClass("test", Map("b" -> StringType))
    val classC = CaseClass("test", Map("c" -> StringType))
    ClassExtractor.infer(classA, classB, classC).fields.keySet shouldEqual Set("a", "b", "c")
  }

  it should "change type to optional for fields which do not exist in all classes" in {
    val classA = CaseClass("test", Map("a" -> StringType))
    val classB = CaseClass("test", Map("a" -> StringType, "b" -> StringType))
    ClassExtractor.infer(classA, classB).fields shouldEqual Map("a" -> StringType, "b" -> OptionalType(StringType))
  }

  "CaseClass getClasses" should "return the same class if there is no nested class" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    ClassExtractor.extract(testClass) should contain theSameElementsAs Seq(testClass)
  }

  it should "return all nested classes (one level)" in {
    val nestedClassB = CaseClass("b", Map("b" -> StringType))
    val nestedClassC = CaseClass("c", Map("c" -> StringType))
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "c" -> nestedClassC))
    ClassExtractor.extract(testClass) should contain theSameElementsAs Seq(testClass, nestedClassB, nestedClassC)
  }

  it should "return all nested classes (two levels)" in {
    val nestedClassB = CaseClass("b", Map("b" -> StringType))
    val nestedClassC = CaseClass("c", Map("c" -> StringType))
    val nestedClassD = CaseClass("d", Map("d" -> nestedClassC))
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "d" -> nestedClassD))
    ClassExtractor.extract(testClass) should contain theSameElementsAs Seq(testClass, nestedClassB, nestedClassC, nestedClassD)
  }

  it should "extract all classes from two identical root classes" in {
    val nestedClassB = CaseClass("b", Map("b" -> StringType))
    val nestedClassC = CaseClass("c", Map("c" -> StringType))
    val nestedClassD = CaseClass("d", Map("d" -> nestedClassC))
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "d" -> nestedClassD))

    ClassExtractor(List(testClass, testClass)) should contain only(testClass, nestedClassB, nestedClassC, nestedClassD)
  }

  it should "extract all classes from two different root classes" in {
    val nestedClassB = CaseClass("b", Map("b" -> StringType))
    val nestedClassC = CaseClass("c", Map("c" -> StringType))
    val nestedClassD = CaseClass("d", Map("d" -> nestedClassC))
    val testClass1 = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "d" -> nestedClassD))

    val nestedClassE = CaseClass("e", Map("e" -> StringType))
    val testClass2 = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "e" -> nestedClassE))

    val expectedRootClass = CaseClass("test",
      Map("a" -> StringType, "b" -> nestedClassB, "d" -> OptionalType(nestedClassD), "e" -> OptionalType(nestedClassE)))

    ClassExtractor(List(testClass1, testClass2)) should contain only(expectedRootClass, nestedClassB, nestedClassC, nestedClassD, nestedClassE)

  }

}
