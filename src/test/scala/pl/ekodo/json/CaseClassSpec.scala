package pl.ekodo.json

import org.scalatest.{FlatSpec, Matchers}

class CaseClassSpec extends FlatSpec with Matchers {

  "CaseClass inference" should "return the same class if only one class is passed" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    CaseClass.infer(testClass) shouldEqual testClass
  }

  it should "return the same class if all passed classes have the same structure" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    CaseClass.infer(testClass, testClass, testClass, testClass) shouldEqual testClass
  }

  it should "throw an exception if passed classes have different names" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    val otherClass = CaseClass("other", Map("a" -> StringType, "b" -> IntType))
    intercept[IllegalArgumentException] {
      CaseClass.infer(testClass, otherClass) shouldEqual testClass
    }
  }

  it should "collect fields from all classes" in {
    val classA = CaseClass("test", Map("a" -> StringType))
    val classB = CaseClass("test", Map("b" -> StringType))
    val classC = CaseClass("test", Map("c" -> StringType))
    CaseClass.infer(classA, classB, classC).fields.keySet shouldEqual Set("a", "b", "c")
  }

  it should "change type to optional for fields which do not exist in all classes" in {
    val classA = CaseClass("test", Map("a" -> StringType))
    val classB = CaseClass("test", Map("a" -> StringType, "b" -> StringType))
    CaseClass.infer(classA, classB).fields shouldEqual Map("a" -> StringType, "b" -> OptionalType(StringType))
  }

  "CaseClass getClasses" should "return the same class if there is no nested class" in {
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> IntType))
    CaseClass.getClasses(testClass) shouldEqual Set(testClass)
  }

  it should "return all nested classes (one level)" in {
    val nestedClassB = CaseClass("b", Map("b" -> StringType))
    val nestedClassC = CaseClass("c", Map("c" -> StringType))
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "c" -> nestedClassC))
    CaseClass.getClasses(testClass) shouldEqual Set(testClass, nestedClassB, nestedClassC)
  }

  it should "return all nested classes (two levels)" in {
    val nestedClassB = CaseClass("b", Map("b" -> StringType))
    val nestedClassC = CaseClass("c", Map("c" -> StringType))
    val nestedClassD = CaseClass("d", Map("d" -> nestedClassC))
    val testClass = CaseClass("test", Map("a" -> StringType, "b" -> nestedClassB, "d" -> nestedClassD))
    CaseClass.getClasses(testClass) shouldEqual Set(testClass, nestedClassB, nestedClassC, nestedClassD)
  }


}
