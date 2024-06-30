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

  def asJavaScriptString(expression: ICFP): String =
    expression match {
      case ICFP.Boolean(value) =>
        if (value) "true" else "false"

      case ICFP.Integer(value) =>
        s"$value"

      case ICFP.String(value) =>
        s"$value"

      case ICFP.Variable(value) =>
        s"v$value"

      case ICFP.Expression.Unary(ICFP.Operator.Unary.LambdaAbstraction(variable), expression) =>
        s"(v$variable => ${asJavaScriptString(expression)})"

      case ICFP.Expression.Unary(operator, expression) =>
        val opCode = operator match {
          case ICFP.Operator.Unary.Negate      => "-"
          case ICFP.Operator.Unary.Not         => "!"
          case ICFP.Operator.Unary.StringToInt => "stringToInt"
          case ICFP.Operator.Unary.IntToString => "intToString"
        }

        s"$opCode(${asJavaScriptString(expression)})"

      case ICFP.Expression.Binary(ICFP.Operator.Binary.LambdaApplication, lhs, rhs) =>
        s"${asJavaScriptString(lhs)}(${asJavaScriptString(rhs)})"

      case ICFP.Expression.Binary(ICFP.Operator.Binary.Take, lhs, rhs) =>
        s"${asJavaScriptString(rhs)}.substr(0, ${asJavaScriptString(lhs)})"

      case ICFP.Expression.Binary(ICFP.Operator.Binary.Drop, lhs, rhs) =>
        s"${asJavaScriptString(rhs)}.substr(${asJavaScriptString(lhs)})"

      case ICFP.Expression.Binary(operator, lhs, rhs) =>
        val opCode = operator match {
          case ICFP.Operator.Binary.Add           => "+"
          case ICFP.Operator.Binary.Subtract      => "-"
          case ICFP.Operator.Binary.Multiply      => "*"
          case ICFP.Operator.Binary.Divide        => "/"
          case ICFP.Operator.Binary.Modulo        => "%"
          case ICFP.Operator.Binary.IsLessThan    => "<"
          case ICFP.Operator.Binary.IsGreaterThan => ">"
          case ICFP.Operator.Binary.IsEqual       => "=="
          case ICFP.Operator.Binary.Or            => "||"
          case ICFP.Operator.Binary.And           => "&&"
          case ICFP.Operator.Binary.Concatenate   => "+"
        }

        s"(${asJavaScriptString(lhs)} $opCode ${asJavaScriptString(rhs)})"

      case ICFP.Expression.Ternary(ICFP.Operator.Ternary.If, condition, ifTrue, ifFalse) =>
        s"(${asJavaScriptString(condition)} ? ${asJavaScriptString(ifTrue)} : ${asJavaScriptString(ifFalse)})"
    }

  def asLispyString(expression: ICFP): String =
    expression match {
      case ICFP.Boolean(value) =>
        if (value) "true" else "false"

      case ICFP.Integer(value) =>
        s"$value"

      case ICFP.String(value) =>
        s"$value"

      case ICFP.Variable(value) =>
        s"v$value"

      case ICFP.Expression.Unary(operator, expression) =>
        val opCode = operator match {
          case ICFP.Operator.Unary.Negate                      => "-"
          case ICFP.Operator.Unary.Not                         => "!"
          case ICFP.Operator.Unary.StringToInt                 => "string-to-int"
          case ICFP.Operator.Unary.IntToString                 => "int-to-string"
          case ICFP.Operator.Unary.LambdaAbstraction(variable) => s"v$variable =>"
        }

        s"($opCode ${asLispyString(expression)})"

      case ICFP.Expression.Binary(ICFP.Operator.Binary.LambdaApplication, lhs, rhs) =>
        s"(${asLispyString(lhs)} ${asLispyString(rhs)})"

      case ICFP.Expression.Binary(operator, lhs, rhs) =>
        val opCode = operator match {
          case ICFP.Operator.Binary.Add           => "+"
          case ICFP.Operator.Binary.Subtract      => "-"
          case ICFP.Operator.Binary.Multiply      => "*"
          case ICFP.Operator.Binary.Divide        => "/"
          case ICFP.Operator.Binary.Modulo        => "%"
          case ICFP.Operator.Binary.IsLessThan    => "<"
          case ICFP.Operator.Binary.IsGreaterThan => ">"
          case ICFP.Operator.Binary.IsEqual       => "="
          case ICFP.Operator.Binary.Or            => "or"
          case ICFP.Operator.Binary.And           => "and"
          case ICFP.Operator.Binary.Concatenate   => "concat"
          case ICFP.Operator.Binary.Take          => "take"
          case ICFP.Operator.Binary.Drop          => "drop"
        }

        s"($opCode ${asLispyString(lhs)} ${asLispyString(rhs)})"

      case ICFP.Expression.Ternary(ICFP.Operator.Ternary.If, condition, ifTrue, ifFalse) =>
        s"(if ${asLispyString(condition)} ${asLispyString(ifTrue)} ${asLispyString(ifFalse)})"
    }
}
