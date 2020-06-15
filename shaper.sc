#!/usr/bin/env amm

import ammonite.ops
import upickle.default.{macroRW, MapStringReader}
import $file.src.{errors, types, template}
import errors._
import types._
import template._

import scala.util.Try

object Shape {
    case class Conf(
        params: Params = Map.empty,
        ignore: Seq[RelPath] = Seq.empty
    )

    object Conf {
        implicit val rw = macroRW[Conf]

        def load(shapeFile: Path): Either[ShapeError, Shape.Conf] = {
            Try {
                upickle.default.read[Conf](read(shapeFile))
            }.toEither.left.map {
                case _: java.nio.file.NoSuchFileException => NoShapeJsonFile(shapeFile)
            }
        }
    }

    case class TemplateFile(
        path: TemplatePath,
        op: TemplateOp,
        keys: Set[String],
    )

    def gatherTemplateKeys(path: Path): Either[ShapeError, Set[String]] = {
        Try {
            read(path)
        }.toEither
         .left.map(t => UnreadableShapeFile(path, t.toString))
         .map(TemplateString.apply)
         .map(_.keys)
    }

    def collectTemplateFiles(where: Path, ignored: Seq[RelPath]): Either[ShapeError, Seq[TemplateFile]] = {
        Try {
            ls.rec(where)
                .filterNot(_.isDir)
                .map(path => path.relativeTo(where))
                .filter(_ != RelPath("shape.json"))
                .filterNot(ignored.contains)
                .map {
                    path => gatherTemplateKeys(where / path).map(_.concat(TemplatePath(path).keys)).map(path -> _)
                }
                .map {
                    case Right((path, keys)) if path.ext.equals("append") =>
                        TemplateFile(TemplatePath(path / up / path.baseName), TemplateOp.Append, keys)
                    case Right((path, keys)) =>
                        TemplateFile(TemplatePath(path), TemplateOp.NewFile, keys)
                }
        }.toEither.left.map {
            case t: ShapeError => t
            case t: Throwable => UnknownError(t)
        }
    }

    def load(shapeLoc: Path): Either[ShapeError, Shape] = {
        for {
            conf <- Conf.load(shapeLoc / "shape.json")
            files <- collectTemplateFiles(shapeLoc, conf.ignore)
        } yield Shape(shapeLoc, conf, files)
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

case class ShapeTarget(source: Path, target: Path, op: TemplateOp)

case class Shape(loc: Path, conf: Shape.Conf, files: Seq[Shape.TemplateFile]) {
    def realize(to: Path, params: Params): Either[ShapeError, Seq[Path]] = {
        for {
            _ <- validateTemplates(params)
            targets <- generateTargets(to, params)
            _ <- targets.collect(invalidTargets) match {
                case invalids if invalids.nonEmpty =>
                    Left(
                        invalids.fold(UnrealizableFiles(Map.empty)) {
                            case (acc: UnrealizableFiles, err: UnrealizableFile) =>
                                acc.updated(err)
                        })
                case _ => Right(())
            }
            files <- targets.map(realizeTarget(params)).partition(_.isRight) match {
                case (_, errors) if errors.nonEmpty =>
                    Left(UnrealizableFiles(errors.map {
                        case Left(UnrealizableFile(path, reason)) => path -> reason
                    }.toMap))
                case (paths, _) => Right(paths.collect { case Right(path) => path })
            }
        } yield files
    }

    def validateTemplates(params: Params): Either[ShapeError, Unit] = {
        val paramKeys = params.keys
        files
            .map(file => file -> (file.keys -- paramKeys))
            .filter(_._2.size > 0)
            .map {
                case (file, excessKeys) =>
                    (loc / file.path.value) -> s"missing keys: ${excessKeys.mkString(",")}"
            } match {
                case errors if errors.nonEmpty => Left(UnrealizableFiles(errors.toMap))
                case _ => Right(())
            }
    }

    def generateTargets(to: Path, params: Params): Either[ShapeError, Seq[ShapeTarget]] = {
        files.map(file => (
                file.path.realize(params).map { realizedPath => to / realizedPath},
                (loc / file.path.value),
                file.op
            )
        ).partition(_._1.isRight) match {
            case (_, errors) =>
                Left(errors.map {
                    case (Left(t: ShapeError), _, _) => t
                    case (Left(t: Throwable), source, _) => UnrealizableFile(source, s"could not realize file $t")
                }.fold(UnrealizableFiles(Map.empty)) {
                    case (acc: UnrealizableFiles, err: UnrealizableFile) =>
                        acc.updated(err)
                })
            case (successes, _) =>
                Right(successes.collect {
                    case (Right(target), source, op) => ShapeTarget(target, source, op)
                })
        }
    }

    def invalidTargets: PartialFunction[ShapeTarget, UnrealizableFile] = {
        case ShapeTarget(target, source, TemplateOp.NewFile) if exists(target) =>
            UnrealizableFile(source, s"$target already exists")
        case ShapeTarget(target, source, TemplateOp.Append) if !exists(target) =>
            UnrealizableFile(source, s"$target does not exist")
    }

    def realizeTarget(params: Params): ShapeTarget => Either[ShapeError, Path] = {
        case ShapeTarget(target, source, TemplateOp.NewFile) =>
            Try(read(source)).toEither
                .left.map(t => UnrealizableFile(source, s"could not read source ${t.toString}"))
                .map(TemplateString.apply)
                .flatMap(_.realize(params))
                .flatMap(content =>
                    Try(write(target, content, createFolders=true))
                        .toEither
                        .left.map(t => UnrealizableFile(source, s"could not read source ${t.toString}"))
                        .map(_ => target)
                )
    }
}

/*
case class Shape(
    srcLoc: Path,
    conf: Shape.Conf,
    templates: Seq[Shape.TemplateFile]
) {
    type RealizedPath = (Path, Path, TemplateOps)

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
                        case TemplateOps.Append => srcLoc / RelPath(path.toString ++ ".append")
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
        case (targetPath, sourcePath, TemplateOps.NewFile) if exists(targetPath) =>
            Left(UnrealizableFile(sourcePath, s"$targetPath already exists"))
        case (targetPath, sourcePath, TemplateOps.Append) if !exists(targetPath) =>
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

    def writeTargetFile(op: TemplateOps)(content: String, path: Path): Either[ShapeError, Unit] = {
        op match {
            case TemplateOps.NewFile =>
                Try(write(path, content)).toEither.left.map {
                    case t: Throwable => UnknownError(t)
                }
            case TemplateOps.Append =>
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
*/