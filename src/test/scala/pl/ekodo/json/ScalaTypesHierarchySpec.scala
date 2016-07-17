package pl.ekodo.json

import org.scalatest.{FlatSpec, Matchers}

class ScalaTypesHierarchySpec extends FlatSpec with Matchers {

  "ScalaTypesHierarchy" should "return the same type if passes types are equal" in {
    ScalaTypesHierarchy.commonParent(IntType, IntType) shouldEqual IntType
  }

  it should "return parent type if parent type and direct descendant type is passed" in {
    ScalaTypesHierarchy.commonParent(LongType, IntType) shouldEqual LongType
  }

  it should "return parent type if parent type and descendant type is passed" in {
    ScalaTypesHierarchy.commonParent(BigDecimalType, IntType) shouldEqual BigDecimalType
  }

  it should "return common parent type if exists" in {
    ScalaTypesHierarchy.commonParent(DoubleType, IntType) shouldEqual BigDecimalType
  }

  it should "return AnyType if parent type does not exist" in {
    ScalaTypesHierarchy.commonParent(StringType, IntType) shouldEqual AnyType
  }

}
