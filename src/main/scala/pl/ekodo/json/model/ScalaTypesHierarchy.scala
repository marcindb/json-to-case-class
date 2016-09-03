package pl.ekodo.json.model

import scala.annotation.tailrec

/**
  * Simple representation of types hierarchy
  */
object ScalaTypesHierarchy {

  private val hierarchy = Map[ScalaType, Option[ScalaType]](
    AnyType -> None,
    StringType -> Some(AnyType),
    BooleanType -> Some(AnyType),
    BigDecimalType -> Some(AnyType),
    DoubleType -> Some(BigDecimalType),
    LongType -> Some(BigDecimalType),
    IntType -> Some(LongType)
  ).withDefaultValue(Some(AnyType))

  /**
    * Infers minimal common parent for given scala types
    *
    * @param types  scala types
    * @return       minimal parent for given types
    */
  def commonParent(types: List[ScalaType]): ScalaType =
    types.
      map(parents).
      reduce((t1, t2) => t1.intersect(t2)).
      head

  private def parents(t: ScalaType): List[ScalaType] = {
    @tailrec
    def loop(t: ScalaType, acc: List[ScalaType]): List[ScalaType] = {
      val parentType = hierarchy(t)
      parentType match {
        case Some(pt) => loop(pt, acc :+ pt)
        case None => acc
      }
    }
    loop(t, List(t))
  }

}
