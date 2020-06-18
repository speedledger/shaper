import $file.src.template
import template._

def testExtractKeys: Unit = {
    Seq(
        "$" -> Set(),
        "${apa} ${bepa}" -> Set("apa", "bepa"),
        "\\${apa} ${bepa}" -> Set("bepa"),
        "\\${apa}" -> Set(),
        "test\\${apa}${cepa}" -> Set("cepa"),
        "test\\${apa}$${cepa}" -> Set("cepa"),
        "test\\$${apa}$$$${cepa}" -> Set("apa", "cepa"),
        "\\$${foo}" -> Set("foo"),
        "$$${foo}" -> Set("foo"),
    ).map {
        case (a, b) =>
            val r = TemplateString(a).keys
            if (r == b) {
                true
            } else {
                System.err.println(s"$r != $b")
                false
            }
    }.filterNot(identity) match {
        case errors if errors.length > 0 =>
            throw new RuntimeException(s"${errors.length} tests failed")
        case _ =>
            ()
    }
}

def testTemplateString: Unit = {
    val params = Map[String, String](
        "foo" -> "APA",
        "bar" -> "TukTuk",
        "semiTemp" -> "${foo}"
        )
    Seq(
        "${" -> Right("${"),
        "${}" -> Right("${}"),
        "\\${foo}" -> Right("${foo}"),
        "${foo}" -> Right("APA"),
        "pre${foo}" -> Right("preAPA"),
        "${foo}suf" -> Right("APAsuf"),
        "${foo}in${foo}" -> Right("APAinAPA"),
        "${flopp}" -> Left("UnknownKeys(Set(flopp))"),
        "${semiTemp}" -> Right("${foo}")
    ).map {
        case (a, b) =>
            val r = TemplateString(a).realize(params)
                .left.map(_.toString)
            if (r == b) {
                true
            } else {
                System.err.println(s"$r != $b")
                false
            }
    }.filterNot(identity) match {
        case errors if errors.length > 0 =>
            throw new RuntimeException(s"${errors.length} tests failed")
        case _ =>
            ()
    }
}

@main
def test() = {
    testExtractKeys
    testTemplateString
}