package pl.ekodo.json.model

import org.scalatest.{FlatSpec, Matchers}

class ScalaTypesHierarchySpec extends FlatSpec with Matchers {

  "ScalaTypesHierarchy" should "return the same type if passes types are equal" in {
    ScalaTypesHierarchy.commonParent(List(IntType, IntType)) shouldEqual IntType
  }

  it should "return parent type if parent type and direct descendant type is passed" in {
    ScalaTypesHierarchy.commonParent(List(LongType, IntType)) shouldEqual LongType
  }

  it should "return parent type if parent type and descendant type is passed" in {
    ScalaTypesHierarchy.commonParent(List(BigDecimalType, IntType)) shouldEqual BigDecimalType
  }

  it should "return common parent type if exists" in {
    ScalaTypesHierarchy.commonParent(List(DoubleType, IntType)) shouldEqual BigDecimalType
  }

  it should "return AnyType if parent type does not exist" in {
    ScalaTypesHierarchy.commonParent(List(StringType, IntType)) shouldEqual AnyType
  }

}
