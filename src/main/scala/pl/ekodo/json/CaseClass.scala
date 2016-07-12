package pl.ekodo.json

import scala.annotation.tailrec

case class CaseClass(name: String, fields: Map[String, ScalaType]) extends ScalaType

object CaseClass {

  def apply(caseClasses: Seq[CaseClass]): Set[CaseClass] = {
    val allClasses = caseClasses.map(extract).reduce(_ ++ _)
    allClasses.groupBy(_.name).mapValues(s => infer(s.head, s.tail: _*)).values.toSet
  }

  def extract(caseClass: CaseClass): List[CaseClass] = {
    @tailrec
    def loop(caseClasses: List[CaseClass], acc: List[CaseClass] = List[CaseClass]()): List[CaseClass] = {
      val classes = caseClasses.flatMap(_.fields.collect {
        case (name, cc: CaseClass) => cc
        case (name, SeqType(cc: CaseClass)) => cc
      }.toList)
      if(classes.isEmpty) acc ++ caseClasses else loop(classes, acc ++ caseClasses)
    }
    loop(List(caseClass))
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
