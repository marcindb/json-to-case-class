package pl.ekodo.json.transformation

import pl.ekodo.json.model._

import scala.annotation.tailrec

/**
  * Extracts common classes from root classes.
  */
object ClassExtractor {

  /**
    * Extracts common classes from root classes.
    *
    * @param caseClasses  root classes representing the same json object
    * @return             common classes extracted from caseClasses
    */
  def apply(caseClasses: Iterable[CaseClass]): Iterable[CaseClass] = {
    val allClasses = caseClasses.map(extract).reduce(_ ++ _)
    allClasses.groupBy(c => (c.name, c.parents)).mapValues(s => infer(s.head, s.tail: _*)).values
  }

  def extract(caseClass: CaseClass): List[CaseClass] = {
    @tailrec
    def loop(caseClasses: List[CaseClass], acc: List[CaseClass] = List[CaseClass]()): List[CaseClass] = {
      val classes = caseClasses.flatMap(_.fields.collect {
        case (_, cc: CaseClass) => cc
        case (_, SeqType(cc: CaseClass)) => cc
        case (_, OptionalType(cc: CaseClass)) => cc
      }.toList)
      if (classes.isEmpty) acc ++ caseClasses else loop(classes, acc ++ caseClasses)
    }
    loop(List(caseClass))
  }

  def infer(caseClass: CaseClass, caseClasses: CaseClass*): CaseClass = {
    val name = caseClass.name
    require(caseClasses.forall(_.name == name), "All classes must have the same name.")
    val allClasses = caseClass +: caseClasses
    val size = allClasses.size
    val fields = allClasses
      .foldLeft(List[(String, ScalaType)]())((acc, cc) => acc ++ cc.fields.toList)
      .groupBy(_._1)
      .map { case (k, v) =>
        val inferredType = ScalaTypesHierarchy.commonParent(v.map(_._2))
        (k, inferredType) -> v.size
      }

    val inferredFields = fields.collect {
      case (key, count) if count == size => key
      case ((n, scalaType), _) => n -> OptionalType(scalaType)
    }

    CaseClass(name, inferredFields, caseClass.parents)
  }

}

