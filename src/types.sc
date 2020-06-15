import ammonite.ops.RelPath
import upickle.default._

type Params = Map[String, String]

sealed trait TemplateOp
object TemplateOp {
    case object Append extends TemplateOp
    case object NewFile extends TemplateOp
}

implicit val relPathRW = upickle
    .default
    .readwriter[String]
    .bimap[RelPath](
        x => x.toString,
        str => RelPath(str)
    )

