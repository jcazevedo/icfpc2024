package net.jcazevedo.icfpc2024.lang

import fastparse.MultiLineWhitespace._
import fastparse._

object Parser {
  private def boolean[$: P]: P[ICFP.Boolean] =
    P(("T" | "F").!).collect({
      case "T" => ICFP.Boolean(true)
      case "F" => ICFP.Boolean(false)
    })

  private def integer[$: P]: P[ICFP.Integer] =
    P("I" ~~ CharsWhile(_ != ' ').!).map(digits => ICFP.Integer(ICFP.Integer.fromBase94(digits)))

  private def string[$: P]: P[ICFP.String] =
    P("S" ~~ CharsWhile(_ != ' ').?.!).map(str => ICFP.String(ICFP.String.fromHuman(str)))

  private def variable[$: P]: P[ICFP.Variable] =
    P("v" ~~ CharsWhile(_ != ' ').!).map(digits => ICFP.Variable(ICFP.Integer.fromBase94(digits).toInt))

  private def unaryOp[$: P]: P[ICFP.Operator.Unary] =
    P(CharsWhile(_ != ' ').!).collect({
      case "U-"                => ICFP.Operator.Unary.Negate
      case "U!"                => ICFP.Operator.Unary.Not
      case "U#"                => ICFP.Operator.Unary.StringToInt
      case "U$"                => ICFP.Operator.Unary.IntToString
      case s"L$variableNumber" => ICFP.Operator.Unary.LambdaAbstraction(ICFP.Integer.fromBase94(variableNumber).toInt)
    })

  private def unary[$: P]: P[ICFP.Expression.Unary] =
    P(unaryOp ~ expression).map({ case (op, expr) => ICFP.Expression.Unary(op, expr) })

  private def binaryOp[$: P]: P[ICFP.Operator.Binary] =
    P(CharsWhile(_ != ' ').!).collect({
      case "B+" => ICFP.Operator.Binary.Add
      case "B-" => ICFP.Operator.Binary.Subtract
      case "B*" => ICFP.Operator.Binary.Multiply
      case "B/" => ICFP.Operator.Binary.Divide
      case "B%" => ICFP.Operator.Binary.Modulo
      case "B<" => ICFP.Operator.Binary.IsLessThan
      case "B>" => ICFP.Operator.Binary.IsGreaterThan
      case "B=" => ICFP.Operator.Binary.IsEqual
      case "B|" => ICFP.Operator.Binary.Or
      case "B&" => ICFP.Operator.Binary.And
      case "B." => ICFP.Operator.Binary.Concatenate
      case "BT" => ICFP.Operator.Binary.Take
      case "BD" => ICFP.Operator.Binary.Drop
      case "B$" => ICFP.Operator.Binary.LambdaApplication
    })

  private def binary[$: P]: P[ICFP.Expression.Binary] =
    P(binaryOp ~ expression ~ expression).map({ case (op, lhs, rhs) => ICFP.Expression.Binary(op, lhs, rhs) })

  private def ternaryOp[$: P]: P[ICFP.Operator.Ternary] =
    P(CharsWhile(_ != ' ').!).collect({ case "?" =>
      ICFP.Operator.Ternary.If
    })

  private def ternary[$: P]: P[ICFP.Expression.Ternary] =
    P(ternaryOp ~ expression ~ expression ~ expression).map({ case (op, exp1, exp2, exp3) =>
      ICFP.Expression.Ternary(op, exp1, exp2, exp3)
    })

  private def expression[$: P]: P[ICFP] =
    P(boolean | integer | string | variable | unary | binary | ternary)

  def parse(str: String): ICFP =
    fastparse.parse(str, expression(_)) match {
      case Parsed.Success(result, _) => result
      case f: Parsed.Failure         => throw new Exception(f.msg)
    }
}
