package net.jcazevedo.icfpc2024.lang

import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Unary.{IntToString, Negate, Not, StringToInt}

object Interpreter {
  def unaryOp(op: ICFP.Operator.Unary, value: => ICFP.Atom): ICFP.Atom =
    (op, value) match {
      case (IntToString, int @ ICFP.Integer(value)) if value >= 0 =>
        ICFP.String(int.toString().stripPrefix("I"))

      case (Negate, ICFP.Integer(value)) =>
        ICFP.Integer(-value)

      case (Not, ICFP.Boolean.True) =>
        ICFP.Boolean.False

      case (Not, ICFP.Boolean.False) =>
        ICFP.Boolean.True

      case (StringToInt, ICFP.String(digits)) =>
        ICFP.Integer(digits.foldLeft(0L)((ans, digit) => ans * 94 + (digit.toInt - 33)))

      case _ =>
        throw new IllegalArgumentException(s"Can't apply $op to $value")
    }

  def evaluate(expression: ICFP): ICFP.Atom =
    expression match {
      case atom: ICFP.Atom =>
        atom

      case ICFP.Expression.Unary(op, value) =>
        unaryOp(op, evaluate(value))

      case other =>
        ???
    }
}
