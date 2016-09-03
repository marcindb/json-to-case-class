package pl.ekodo.json.model

/**
  * Phantom trait for scala types
  */
trait ScalaType

case class CaseClass(
  name: String,
  fields: Map[String, ScalaType],
  parents: List[String] = List.empty,
  replace: List[CaseClass] = List.empty) extends ScalaType

case object AnyType extends ScalaType

case class SeqType(scalaType: ScalaType) extends ScalaType

case object StringType extends ScalaType

case object IntType extends ScalaType

case object LongType extends ScalaType

case object DoubleType extends ScalaType

case object BigDecimalType extends ScalaType

case object BooleanType extends ScalaType

case class OptionalType(scalaType: ScalaType) extends ScalaType




