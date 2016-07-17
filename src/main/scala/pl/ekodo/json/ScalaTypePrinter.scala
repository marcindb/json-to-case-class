package pl.ekodo.json

object ScalaTypePrinter {

  def apply(cc: CaseClass): String = {
    val sb = new StringBuilder
    sb.append(s"case class ${cc.name}(\n")
    val fields = cc.fields.map { case (k, v) => s"  $k: ${print(v)}" }.mkString(",\n")
    sb.append(fields)
    sb.append("\n)\n")
    sb.toString()
  }

  private def print(scalaType: ScalaType): String = scalaType match {
    case AnyType => "Any"
    case BigDecimalType => "BigDecimal"
    case BooleanType => "Boolean"
    case DoubleType => "Double"
    case IntType => "Int"
    case LongType => "Long"
    case StringType => "String"
    case cc: CaseClass => cc.name
    case opt: OptionalType => s"Option[${print(opt.scalaType)}]"
    case st: SeqType => s"List[${print(st.scalaType)}]"
  }

}
