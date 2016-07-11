package pl.ekodo.json

import scala.annotation.tailrec

case class CaseClass(name: String, fields: Map[String, ScalaType]) extends ScalaType

object CaseClass {

  //TODO @tailrec
  def getClasses(caseClass: CaseClass): Set[CaseClass] = {
    val classes = caseClass.fields.collect {
      case (name, cc: CaseClass) => cc
      case (name, SeqType(cc: CaseClass))  => cc
    }.toList
    val c = classes.flatMap(getClasses)
    (caseClass +: (classes ++ c)).toSet
  }

  //TODO infer basic type
  def infer(caseClass: CaseClass, caseClasses: CaseClass*): CaseClass = {
    val name = caseClass.name
    require(caseClasses.forall(_.name == name), "All classes must have the same name.")
    val allClasses = caseClass +: caseClasses
    val size = allClasses.size
    val fields = allClasses
      .foldLeft(List[(String, ScalaType)]())((acc, cc) => acc ++ cc.fields.toList)
      .groupBy(identity)
      .mapValues(_.size)

    val inferredFields = fields.collect {
      case (key, count) if count == size => key
      case ((n, scalaType), _) => n -> OptionalType(scalaType)
    }

    CaseClass(name, inferredFields)
  }

}
