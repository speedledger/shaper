import $file.errors
import $file.types
import errors._
import types._

import ammonite.ops._
import scala.util.Try

trait Template[T] {
    import Template._

    def value: T

    lazy val keys: Set[String] =
        keyPattern
            .findAllMatchIn(value.toString)
            .map(_.group(1))
            .toSet

    def check(params: Params): Either[ShapeError, Unit] = {
        (keys -- params.keys) match {
                case unknownKeys if unknownKeys.nonEmpty => Left(UnknownKeys(unknownKeys))
                case _ => Right(())
        }
    }

    def realize(params: Params): Either[ShapeError, T]
}

object Template {
    val keyPattern = raw"\$$\{([a-zA-Z0-9]+)\}".r
}

case class TemplateString(value: String) extends Template[String] {
    import Template._

    def realize(params: Params): Either[ShapeError, String] = {
        for {
            _ <- check(params)
            relString <- Try {
                keyPattern.replaceAllIn(
                    value,
                    {
                        case keyPattern(tag) =>
                            params.get(tag).getOrElse(throw new Throwable(""))
                    }
                )
            }.toEither.left.map(t => UnknownError(t))
        } yield relString
    }
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