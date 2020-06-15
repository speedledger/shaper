#!/usr/bin/env amm

import ammonite.ops
import upickle.default.{macroRW, MapStringReader}
import scala.util.Try

sealed trait ShapeError
object ShapeError {
    def describe: ShapeError => String = {
        case NoShapeJsonFile(where) => s"could not load $where for shape"
        case UnrealizableFiles(files) => {
            files.map {
                case (file, reason) => s"${file.relativeTo(wd)}: \t$reason"
            }.mkString("\n")
        }
        case t => s"could not realize shape: $t"
    }
}
case class NoSuchShape(name: String) extends ShapeError
case class NoShapeJsonFile(where: Path) extends ShapeError
case class UnknownKeys(keys: Set[String]) extends ShapeError
case class UnrealizableFile(file: Path, reason: String = "") extends ShapeError
case class UnrealizableFiles(files: Map[Path, String]) extends ShapeError {
    def updated(t: UnrealizableFile) = {
        UnrealizableFiles(files.updated(t.file, t.reason))
    }
}
case class UnknownError(t: Throwable) extends ShapeError

implicit val relPathRW = upickle
    .default
    .readwriter[String]
    .bimap[RelPath](
        x => x.toString,
        str => RelPath(str)
    )

case class TKey(value: String) extends AnyVal

type Params = Map[TKey, String]

trait Template[T] {
    def value: T
    def keys: Set[TKey] = {
        keyPattern.findAllMatchIn(value.toString).map(_.group(1)).map(TKey.apply).toSet
    }
    def realize(params: Params): Either[ShapeError, T]
}

object Template {
    val keyPattern = raw"\$$\{([^}]+)\}".r
}

case class TemplateString(value: String) extends Template[String] {
    def realize(params: Params): Either[ShapeError, String] = {
        for {
            _ <- (keys -- params.keys) match {
                case unknownKeys if unknownKeys.isEmpty => Right(())
                case unknownKeys => Left(UnknownKeys(unknownKeys))
            }
            relString <- Try {
                templatePattern.replaceAllIn(
                    value,
                    {
                        case templatePattern(tag) =>
                            params.get(tag).getOrElse(throw new Throwable(""))
                    }
                )
            }.toEither.left.map(t => UnknownError(t))
        } yield relString
    }
}

case class TemplatePath(path: RelPath) extends Template[RelPath] {
    templatePath.segments
    .flatMap(segment => TemplatePath(segment).realize(params))
    .partition(_.isRight) match {
        case (_, badSegments) if badSegments.nonEmpty =>
            badSegments.head.asInstanceOf[Either[ShapeError, RelPath]]
        case (newSegments, _) =>
            val newPath = newSegments.collect { case Right(s) => s }.mkString("/")
            Right(RelPath(newPath))
    }
}

object Shape {
    case class Conf(
        params: Params,
        ignore: Seq[RelPath]
    )
    object Conf {
        implicit val rw = macroRW[Conf]

        def load(shapeFile: Path): Either[ShapeError, Shape.Conf] = {
            Try {
                upickle.default.read[Shape.Conf](read(shapeFile))
            }.toEither.left.map {
                case _: java.nio.file.NoSuchFileException => NoShapeJsonFile(shapeFile)
            }
        }
    }

    sealed trait TemplateType
    object TemplateType {
        case object Append extends TemplateType
        case object NewFile extends TemplateType
    }

    case class TemplateFile(
        path: RelPath,
        templateType: TemplateType,
        params: Set[String]
    )

    def collectTemplateFiles(shapePath: Path, shapeConfig: Shape.Conf): Either[ShapeError, Seq[Shape.TemplateFile]] = {
        Try {
            ls.rec(shapePath)
                .filterNot(_.isDir)
                .map(path => path.relativeTo(shapePath))
                .filter(_ != RelPath("shape.json"))
                .filterNot(shapeConfig.ignore.contains)
                .map {
                    path => path -> identifyKeys(path.toString).concat(identifyKeys(read(shapePath / path)))
                }
                .map {
                    case (path, params) if path.ext.equals("append") =>
                        TemplateFile(path / up / path.baseName, TemplateType.Append, params)
                    case (path, params) =>
                        TemplateFile(path, TemplateType.NewFile, params)
                }
        }.toEither.left.map {
            case t: ShapeError => t
            case t: Throwable => UnknownError(t)
        }
    }

    def load(shapePath: Path): Either[ShapeError, Shape] = {
        val shapeFile = shapePath / "shape.json"
        for {
            shapeConfig <- Conf.load(shapeFile)
            templateFiles <- collectTemplateFiles(shapePath, shapeConfig)
            shape = Shape(shapePath, shapeConfig, templateFiles)
        } yield shape
    }


    def list(where: Path): Seq[Path] = {
        Seq(
            where / ".shapes"
        )
        .filter(exists)
        .flatMap {
            shapesRoot => {
            ls(shapesRoot)
                .filter(path => path.isDir)
            }
        }
    }
}

case class Shape(
    srcLoc: Path,
    conf: Shape.Conf,
    templates: Seq[Shape.TemplateFile]
) {
    type RealizedPath = (Path, Path, Shape.TemplateType)

    def validateTemplateKeys: Shape.TemplateFile => Either[UnrealizableFile, Unit] = {
        case Shape.TemplateFile(_, _, params) if params.subsetOf(conf.params.keys.toSet) => Right(())
        case Shape.TemplateFile(path, _, params) =>
            val missingParams = params -- conf.params.keys
            Left(UnrealizableFile(srcLoc / path, s"missing params: ${missingParams.mkString(",")}"))
    }

    def realTemplateFile(targetLoc: Path): Shape.TemplateFile => Either[UnrealizableFile, RealizedPath] = {
        case Shape.TemplateFile(path, templateType, params) =>
            realizePath(path, conf.params)
                .map { realPath =>
                    val sourcePath = templateType match {
                        case Shape.TemplateType.Append => srcLoc / RelPath(path.toString ++ ".append")
                        case _ => srcLoc / path
                    }
                    (targetLoc / realPath, sourcePath, templateType)
                }
                .left.map {
                    case UnknownKeys(keys) => UnrealizableFile(path, s"unknown keys ${keys.mkString(",")}")
                    case UnknownError(t) => UnrealizableFile(path, t.toString)
                }
    }

    def validateTargetPath: RealizedPath => Either[UnrealizableFile, Unit] = {
        case (targetPath, sourcePath, Shape.TemplateType.NewFile) if exists(targetPath) =>
            Left(UnrealizableFile(sourcePath, s"$targetPath already exists"))
        case (targetPath, sourcePath, Shape.TemplateType.Append) if !exists(targetPath) =>
            Left(UnrealizableFile(sourcePath, s"$targetPath does not exist"))
        case _ =>
            Right(())
    }

    def calculateRealization(targetLoc: Path): Either[ShapeError, Seq[RealizedPath]] = {
        templates
         .map(
             template =>
                for {
                    _ <- validateTemplateKeys(template)
                    realizedPath <- realTemplateFile(targetLoc)(template)
                    _ <- validateTargetPath(realizedPath)
                } yield realizedPath
         )
         .partition(_.isRight) match {
            case (_, invalidPaths) if invalidPaths.nonEmpty =>
                Left(invalidPaths
                    .collect { case Left(e) => e }
                    .fold(UnrealizableFiles(Map.empty))( {
                        case (agg: UnrealizableFiles, err: UnrealizableFile) =>
                            agg.updated(err)
                    }))
            case (realizedPaths, _) =>
                Right(realizedPaths.collect { case Right(v) => v })
         }
    }

    def readTemplate(path: Path): Either[ShapeError, String] = {
        Try(read(path)).toEither.left.map {
            case t: Throwable => UnknownError(t)
        }
    }

    def writeTargetFile(op: Shape.TemplateType)(content: String, path: Path): Either[ShapeError, Unit] = {
        op match {
            case Shape.TemplateType.NewFile =>
                Try(write(path, content)).toEither.left.map {
                    case t: Throwable => UnknownError(t)
                }
            case Shape.TemplateType.Append =>
                Try(write.append(path, content)).toEither.left.map {
                    case t: Throwable => UnknownError(t)
                }
        }
    }

    def makeDirectory(target: Path): Either[ShapeError, Unit] = {
        Try(mkdir(target / up)).toEither.left.map {
            case t: Throwable => UnknownError(t)
        }
    }

    def realize: RealizedPath => Either[ShapeError, Path] = {
        case (targetPath, sourcePath, templateType) =>
            for {
                sourceContent <- readTemplate(sourcePath)
                targetContent <- realizeString(sourceContent, conf.params)
                _ <- makeDirectory(targetPath)
                _ <- writeTargetFile(templateType)(targetContent, targetPath)
            } yield targetPath
    }

    def realizeTo(targetLoc: Path): Either[ShapeError, Seq[Path]] = {
        calculateRealization(targetLoc)
            .flatMap {
                targets =>
                    val (success, failed) = targets.map(realize).partition(_.isRight)
                    if (failed.nonEmpty) {
                        Left(UnrealizableFiles(failed.map {
                            case Left(UnrealizableFile(path, reason)) => path -> reason
                        }.toMap))
                    } else {
                        Right(success.collect { case Right(v) => v })
                    }
            }
    }
}

@main
def realize(shapeName: String) = {
    Shape.list(wd)
        .map(Shape.load)
        .filter(_.exists(_.srcLoc.baseName == shapeName))
        .headOption
        .toRight(NoSuchShape(shapeName).asInstanceOf[ShapeError])
        .joinRight
        .flatMap(_.realizeTo(wd)) match {
            case Right(files) =>
                val fileStrs = files.map(file => s" - $file").mkString("\n")
                System.out.println(s"Updated ${files.length} files\n${fileStrs}")
            case Left(error) =>
                System.err.println(s"Could not apply template:\n${ShapeError.describe(error)}")
                throw ammonite.interp.api.AmmoniteExit(1)
        }
}

// Tests

def test(): Unit = {
    testRealizeString()
}

def testRealizeString(): Unit = {
    val params = Map("foo" -> "bar")
    Seq(
        ("empty string", ("" -> Right(""))),
        ("no var string", ("foobar" -> Right("foobar"))),
        ("wrapped var string", ("${foo}" -> Right("bar"))),
        ("prepended string", ("${foo}apa" -> Right("barapa"))),
        ("appended string", ("apa${foo}" -> Right("apabar"))),
        ("multivar string", ("${foo}apa${foo}" -> Right("barapabar"))),
        ("missing var string", ("${bepa}" -> Left(UnknownKeys(Set("bepa"))))),
        ("mixing existing and missing var string", ("${foo} ${bepa}" -> Left(UnknownKeys(Set("bepa"))))),
    ).map {
        case (desc, (a, b)) => if (realizeString(a, params) == b) { () } else { throw new Throwable(desc) }
    }
}
