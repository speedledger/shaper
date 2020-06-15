import $file.types
import ammonite.ops.{pwd, Path}

sealed trait ShapeError
object ShapeError {
    def describe: ShapeError => String = {
        case NoShapeJsonFile(where) => s"could not load ${where.relativeTo(pwd)} for shape"
        case UnrealizableFiles(files) => {
            files.map {
                case (file, reason) => s"${file}: \t$reason"
            }.mkString("\n")
        }
        case CouldNotParseJson(where: Path, reason: String) =>
            s"could not parse json ${where.relativeTo(pwd)}: $reason"
        case t => s"could not realize shape: $t"
    }
}
case class NoSuchShape(name: String) extends ShapeError
case class NoShapeJsonFile(where: Path) extends ShapeError
case class CouldNotParseJson(where: Path, reason: String) extends ShapeError
case class UnreadableShapeFile(where: Path, reason: String) extends ShapeError
case class UnknownKeys(keys: Set[String]) extends ShapeError
case class UnrealizableFile(file: Path, reason: String = "") extends ShapeError
case class UnrealizableFiles(files: Map[Path, String]) extends ShapeError {
    def updated(t: UnrealizableFile) = {
        UnrealizableFiles(files.updated(t.file, t.reason))
    }
}
case class UnknownError(t: Throwable) extends ShapeError
