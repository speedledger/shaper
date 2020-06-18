import $file.errors
import $file.types
import errors._
import types._

import ammonite.ops._
import scala.util.Try

object Template {
    val keyPattern = raw"(.|)\$$\{(\w+)\}".r
}

trait Template[T] {
    import Template._

    def value: T

    lazy val keys: Set[String] =
        keyPattern
            .findAllMatchIn(value.toString)
            .map(m => m.group(1) -> m.group(2))
            .collect {
                case (notSlash, key) if notSlash != "\\" => key
            }
            .toSet

    def escapeResult(input: String): String = {
        input.replaceAll("\\$", "\\\\\\$")
    }

    def replace(input: String, params: Params): Either[ShapeError, String] = {
        check(params)
            .map(_ =>
                keyPattern.replaceAllIn(
                    input, {
                        case keyPattern("\\", tag) =>
                            escapeResult(s"$${$tag}")
                        case keyPattern(p, tag) =>
                            escapeResult(s"$p${params(tag)}")
                    }
                )
            )
    }

    def check(params: Params): Either[ShapeError, Unit] = {
        (keys -- params.keys) match {
                case unknownKeys if unknownKeys.nonEmpty => Left(UnknownKeys(unknownKeys))
                case _ => Right(())
        }
    }

    def realize(params: Params): Either[ShapeError, T]
}

case class TemplateString(value: String) extends Template[String] {
    override def realize(params: Params): Either[ShapeError,String] = replace(value.toString, params).map(_.toString)
}

case class TemplatePath(value: RelPath) extends Template[RelPath] {
    import Template._

    def realize(params: Params): Either[ShapeError, RelPath] = {
        check(params).flatMap {
            _ =>
                value.segments
                .map(segment => TemplateString(segment).realize(params))
                .partition(_.isRight) match {
                    case (_, badSegments) if badSegments.nonEmpty =>
                        badSegments.head.asInstanceOf[Either[ShapeError, RelPath]]
                    case (newSegments, _) =>
                        val newPath = newSegments.collect { case Right(s) => s }.mkString("/")
                        Right(RelPath(newPath))
                }
        }
    }

}