package net.jcazevedo.icfpc2024.lang

object Utilities {
  final val yCombinator: ICFP =
    Parser.parse("""L! B$ L" B$ v" v" L" B$ v! L# B$ B$ v" v" v#""")

  final def recurse(f: ICFP, input: ICFP): ICFP =
    Parser.parse(s"""B$$ B$$ ${yCombinator.asString} ${f.asString} ${input.asString}""")

  final def stringRepeat(str: String, count: Int): ICFP =
    recurse(
      Parser.parse(s"""L! L" ? B= v" I! S B. ${ICFP.String(str).asString} B$$ v! B- v" I""""),
      ICFP.Integer(count)
    )
}
