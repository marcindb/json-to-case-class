package pl.ekodo.json.transformation

import pl.ekodo.json.model._
import spray.json._

/**
  * Parses json string and unmarschall to [[pl.ekodo.json.model.CaseClass]]
  */
object JsonUnmarschaller {

  /**
    * Unmarschall json string to [[pl.ekodo.json.model.CaseClass]]
    *
    * @param rootClass name of root class
    * @param json      json object
    * @return CaseClass representing given json string
    */
  def apply(rootClass: String, json: String): CaseClass = {
    val obj = json.parseJson.asJsObject
    toCaseClass(rootClass, List.empty, obj)
  }

  private def toScalaType(name: String, parents: List[String], jsValue: JsValue): ScalaType = jsValue match {
    case obj: JsObject => toCaseClass(name, parents, obj)
    case arr: JsArray => toSeq(name, parents, arr)
    case value: JsValue => toBasicType(value)
  }

  private def toCaseClass(name: String, parents: List[String], jsObject: JsObject): CaseClass = {
    val fields = jsObject.fields.map {
      case (k, v) => k -> toScalaType(k, parents :+ name, v)
    }
    CaseClass(name.capitalize, fields, parents)
  }

  //TODO handle seq with hetergonous types
  private def toSeq(name: String, parents: List[String], jsArray: JsArray) = {
    val elems = jsArray.elements.map(toScalaType(name, parents, _)).toList
    elems match {
      case head :: tail if head.isInstanceOf[CaseClass] => SeqType(head)
      case head :: tail => SeqType(head)
      case _ => SeqType(AnyType)
    }
  }


  private def toBasicType(jsValue: JsValue): ScalaType = jsValue match {
    case JsNull => AnyType
    case JsTrue | JsFalse => BooleanType
    case JsNumber(v) => toNumberType(v)
    case _: JsString => StringType
    case _ => throw new IllegalStateException
  }

  private def toNumberType(v: BigDecimal): ScalaType =
    if (v.scale > 0) DoubleType
    else if (v.isValidInt) IntType
    else if (v.isValidLong) LongType
    else BigDecimalType

}
