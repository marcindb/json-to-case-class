package pl.ekodo.json

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
