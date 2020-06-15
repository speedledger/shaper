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
                        TemplateFile(TemplatePath(path), TemplateOp.Append, keys)
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
            files <- targets.map(realizeTarget(params, _)).partition(_.isRight) match {
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
                file.path.realize(params).map {
                    case realizedPath if file.op == TemplateOp.Append => {
                        val completePath = to / realizedPath
                        completePath / up / completePath.baseName // Remove .append suffix
                    }
                    case realizedPath => to / realizedPath
                },
                (loc / file.path.value),
                file.op
            )
        ).partition(_._1.isRight) match {
            case (_, errors) if errors.nonEmpty =>
                Left(errors.map {
                    case (Left(t: ShapeError), _, _) => t
                    case (Left(t: Throwable), source, _) => UnrealizableFile(source, s"could not realize file $t")
                }.fold(UnrealizableFiles(Map.empty)) {
                    case (acc: UnrealizableFiles, err: UnrealizableFile) =>
                        acc.updated(err)
                })
            case (successes, _) =>
                Right(successes.collect {
                    case (Right(target), source, op) => ShapeTarget(source, target, op)
                })
        }
    }

    def invalidTargets: PartialFunction[ShapeTarget, UnrealizableFile] = {
        case ShapeTarget(source, target, TemplateOp.NewFile) if exists(target) =>
            UnrealizableFile(source, s"$target already exists")
        case ShapeTarget(source, target, TemplateOp.Append) if !exists(target) =>
            UnrealizableFile(source, s"$target does not exist")
    }

    def readSource: ShapeTarget => Either[ShapeError, TemplateString] = {
        case ShapeTarget(source, _, _) =>
            Try(read(source)).toEither
                .left.map(t => UnrealizableFile(source, s"could not read source ${t.toString}"))
                .map(TemplateString.apply)
    }

    def writeTarget(content: String): ShapeTarget => Either[ShapeError, Unit] = {
        case ShapeTarget(source, target, TemplateOp.NewFile) =>
            Try(write(target, content, createFolders=true))
                .toEither
                .left.map(t => UnrealizableFile(source, s"could not write target $target: ${t.toString}"))
        case ShapeTarget(source, target, TemplateOp.Append) =>
            Try(write.append(target, content))
                .toEither
                .left.map(t => UnrealizableFile(source, s"could not update target $target: ${t.toString}"))
    }

    def realizeTarget(params: Params, shapeTarget: ShapeTarget): Either[ShapeError, Path] = {
        for {
            templateString <- readSource(shapeTarget)
            targetContent <- templateString.realize(params)
            _ <- writeTarget(targetContent)(shapeTarget)
        } yield shapeTarget.target
    }
}