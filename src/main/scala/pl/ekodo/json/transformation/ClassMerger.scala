package pl.ekodo.json.transformation

import pl.ekodo.json.model.{CaseClass, ScalaType}

/**
  *  Finds and merges classes with the same structure.
  */
object ClassMerger {

  /**
    * Finds and merges classes with the same structure.
    *
    * @param caseClasses  case classes
    * @return             merged case classes
    */
  def apply(caseClasses: Iterable[CaseClass]): Iterable[CaseClass] = {

    val mergedClasses = caseClasses.
      groupBy(_.fields).
      values.
      map(_.reduce((a1, a2) => a1.copy(replace = a1.replace :+ a2)))

    val mergeMapping = mergedClasses.
      foldLeft(Map.empty[ScalaType, CaseClass])((l, c) => l ++ c.replace.map(n => (n, c)))

    mergedClasses.map(c => c.copy(fields = c.fields.mapValues(st => mergeMapping.getOrElse(st, st))))
  }

}
