package pl.ekodo.json

import java.io.File

import scala.io.Source

object Generator extends App {

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

  parser.parse(args, Config()) match {

    case Some(config) =>
      val parsed = Source
        .fromFile(config.in)
        .getLines
        .map(line => JsonToScala(config.rootClass, line))
        .toSeq
      val caseClasses = CaseClass(parsed)
      FilesGenerator(config.out.toPath, caseClasses)

    case None =>
      sys.exit(0)
  }


}

case class Config(rootClass: String = "Root", in: File = new File("."), out: File = new File("./generated"))
