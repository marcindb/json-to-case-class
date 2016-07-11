package pl.ekodo.json

import spray.json._

object JsonToScala {

  def apply(rootClass: String, obj: JsObject): CaseClass = toCaseClass(rootClass, obj)

  private def toScalaType(name: String, jsValue: JsValue): ScalaType = jsValue match {
    case obj: JsObject => toCaseClass(name, obj)
    case arr: JsArray => toSeq(name, arr)
    case value: JsValue => toBasicType(value)
  }

  private def toCaseClass(name: String, jsObject: JsObject): CaseClass = {
    val fields = jsObject.fields.mapValues {
      toScalaType(name, _)
    }
    CaseClass(name.capitalize, fields)
  }

  private def toSeq(name: String, jsArray: JsArray) = {
    val elems = jsArray.elements.map(toScalaType(name, _))

    SeqType(elems.head)
  }


  private def toBasicType(jsValue: JsValue): ScalaType = jsValue match {
    case JsNull => AnyType
    case JsTrue | JsFalse => BooleanType
    case JsNumber(x) => BigDecimalType
    case JsString(x) => StringType
    case _ => throw new IllegalStateException
  }

}
