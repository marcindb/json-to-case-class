package pl.ekodo.json

import java.io.File

import pl.ekodo.json.files.FilesGenerator
import pl.ekodo.json.model.CaseClass
import pl.ekodo.json.transformation.{ClassExtractor, ClassMerger, JsonUnmarschaller}

import scala.io.Source

object JsonToScala {

  val parser = new scopt.OptionParser[Config]("scopt") {
    head("JSON to Scala case classes")

    opt[String]('r', "root").action((x, c) =>
      c.copy(rootClass = x)).text("name of root class")

    opt[File]('i', "in").required().valueName("<file>").
      action((x, c) => c.copy(in = x)).
      text("in file with json requests")

    opt[File]('o', "out").valueName("<dir>").
      action((x, c) => c.copy(out = x)).
      text("out folder")
  }

  def main(args: Array[String]) =  {
    parser.parse(args, Config()) match {
      case Some(config) =>
        val merged = convert(Source.fromFile(config.in).getLines.toStream, config.rootClass)
        FilesGenerator(config.out.toPath, merged)

      case None =>
        sys.exit(0)
    }
  }

  def convert(jsonLines: Iterable[String], rootClassName: String): Iterable[CaseClass] = {
    val parsed = jsonLines.map(line => JsonUnmarschaller(rootClassName, line)).toStream
    val caseClasses = ClassExtractor(parsed)
    ClassMerger(caseClasses)
  }

}

case class Config(rootClass: String = "Root", in: File = new File("."), out: File = new File("./generated"))
