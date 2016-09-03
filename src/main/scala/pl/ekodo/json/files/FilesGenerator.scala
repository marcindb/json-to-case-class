package pl.ekodo.json.files

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import pl.ekodo.json.model.CaseClass

/**
  * Generates files with case classes
  */
object FilesGenerator {

  /**
    * Generates files with case classes
    *
    * @param outputDir  output dir for generated classes
    * @param classes    classes which have to be generated
    */
  def apply(outputDir: Path, classes: Iterable[CaseClass]): Unit = {
    val dir = prepareOutputFolder(outputDir)
    classes.foreach { cc =>
      val s = ScalaTypePrinter(cc)
      Files.write(dir.resolve(s"${cc.name}.scala"), s.getBytes)
    }
  }

  private def prepareOutputFolder(output: Path) = {
    if (Files.exists(output)) {
      Files.walkFileTree(output, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
    Files.createDirectory(output)
  }

}
