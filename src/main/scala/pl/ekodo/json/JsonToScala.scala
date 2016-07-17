package pl.ekodo.json

import spray.json._

object JsonToScala {

  def apply(rootClass: String, json: String): CaseClass = {
    val obj = json.parseJson.asJsObject
    toCaseClass(rootClass, obj)
  }

  private def toScalaType(name: String, jsValue: JsValue): ScalaType = jsValue match {
    case obj: JsObject => toCaseClass(name, obj)
    case arr: JsArray => toSeq(name, arr)
    case value: JsValue => toBasicType(value)
  }

  private def toCaseClass(name: String, jsObject: JsObject): CaseClass = {
    val fields = jsObject.fields.map {
      case(k,v) => k -> toScalaType(k, v)
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
    case JsNumber(v) => toNumberType(v)
    case _: JsString => StringType
    case _ => throw new IllegalStateException
  }

  private def toNumberType(v: BigDecimal): ScalaType =
    if(v.scale > 0) DoubleType
    else if(v.isValidInt) IntType
    else if(v.isValidLong) LongType
    else BigDecimalType

}
