package pl.ekodo.json

import scala.annotation.tailrec

trait ScalaType

case object AnyType extends ScalaType

case class SeqType(scalaType: ScalaType) extends ScalaType

case object StringType extends ScalaType

case object IntType extends ScalaType

case object LongType extends ScalaType

case object DoubleType extends ScalaType

case object BigDecimalType extends ScalaType

case object BooleanType extends ScalaType

case class OptionalType(scalaType: ScalaType) extends ScalaType

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

  def commonParent(type1: ScalaType, type2: ScalaType): ScalaType = h(type1).intersect(h(type2)).head


  def h(t: ScalaType): List[ScalaType] = {
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


