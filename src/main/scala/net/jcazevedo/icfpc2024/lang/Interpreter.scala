package net.jcazevedo.icfpc2024.lang

import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Binary._
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Unary._

object Interpreter {
  def error(op: ICFP.Operator.Unary, value: ICFP): Exception =
    new IllegalArgumentException(s"Can't apply $op to $value")

  def error(op: ICFP.Operator.Binary, lhs: ICFP, rhs: ICFP): Exception =
    new IllegalArgumentException(s"Can't apply $op to $lhs and $rhs")

  def evaluate(expression: ICFP): ICFP =
    expression match {
      case atom: ICFP.Atom =>
        atom

      case lambda: ICFP.Lambda =>
        lambda

      case ICFP.Expression.Unary(op, value) =>
        (op, evaluate(value)) match {
          case (IntToString, int @ ICFP.Integer(value)) if value >= 0 =>
            ICFP.String(ICFP.String.fromHuman(int.toString.stripPrefix("I")))

          case (Negate, ICFP.Integer(value)) =>
            ICFP.Integer(-value)

          case (Not, ICFP.Boolean(v)) =>
            ICFP.Boolean(!v)

          case (StringToInt, str: ICFP.String) =>
            ICFP.Integer(ICFP.Integer.fromBase94(str.toString().stripPrefix("S")))

          case _ =>
            throw error(op, value)
        }

      case ICFP.Expression.Binary(op, lhs, rhs) =>
        (op, evaluate(lhs), evaluate(rhs)) match {
          case (And, ICFP.Boolean(x), ICFP.Boolean(y)) =>
            ICFP.Boolean(x && y)

          case (Add, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Integer(x + y)

          case (IsGreaterThan, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Boolean(x > y)

          case (IsLessThan, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Boolean(x < y)

          case (Or, ICFP.Boolean(x), ICFP.Boolean(y)) =>
            ICFP.Boolean(x || y)

          case (IsEqual, x, y) =>
            ICFP.Boolean(x == y)

          case (Multiply, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Integer(x * y)

          case (Take, ICFP.Integer(x), ICFP.String(y)) =>
            ICFP.String(y.take(x.toInt))

          case (Modulo, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Integer(x % y)

          case (Subtract, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Integer(x - y)

          case (Concatenate, ICFP.String(x), ICFP.String(y)) =>
            ICFP.String(x + y)

          case (Divide, ICFP.Integer(x), ICFP.Integer(y)) =>
            ICFP.Integer(x / y)

          case (Drop, ICFP.Integer(x), ICFP.String(y)) =>
            ICFP.String(y.drop(x.toInt))

          case _ =>
            throw error(op, lhs, rhs)
        }

      case ICFP.If(condition, whenTrue, whenFalse) =>
        evaluate(condition) match {
          case ICFP.Boolean(value) =>
            if (value) evaluate(whenTrue) else evaluate(whenFalse)

          case other =>
            throw new IllegalArgumentException(s"$condition doesn't evaluate to a boolean")
        }

      case _ => ???
    }
}
