package net.jcazevedo.icfpc2024.lang

import scala.collection.mutable

import net.jcazevedo.icfpc2024.lang.ICFP.Expression.{Binary, Ternary, Unary}
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Binary._
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Ternary.If
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Unary._
import net.jcazevedo.icfpc2024.lang.ICFP.{Operator, Variable}

object Interpreter {
  def error(op: ICFP.Operator.Unary, value: ICFP): Exception =
    new IllegalArgumentException(s"Can't apply $op to $value")

  def error(op: ICFP.Operator.Binary, lhs: ICFP, rhs: ICFP): Exception =
    new IllegalArgumentException(s"Can't apply $op to $lhs and $rhs")

  def error(value: ICFP): Exception =
    new IllegalArgumentException(s"Unexpected value $value")

  sealed trait Result
  case class Final(res: ICFP.Atom) extends Result
  case class More(f: PartialFunction[ICFP.Atom, Result], skip: Int = 0) extends Result

  def opResult(operator: Operator): More = {
    operator match {
      case Drop =>
        More({ case ICFP.Integer(x) => More({ case ICFP.String(y) => Final(ICFP.String(y.drop(x.toInt))) }) })

      case LambdaApplication =>
        ???

      case IsEqual =>
        More({ case x => More({ case y => Final(ICFP.Boolean(x == y)) }) })

      case Add =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Integer(x + y)) }) })

      case Multiply =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Integer(x * y)) }) })

      case Or =>
        More({ case ICFP.Boolean(x) => More({ case ICFP.Boolean(y) => Final(ICFP.Boolean(x || y)) }) })

      case Take =>
        More({ case ICFP.Integer(x) => More({ case ICFP.String(y) => Final(ICFP.String(y.take(x.toInt))) }) })

      case IsLessThan =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Boolean(x < y)) }) })

      case Subtract =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Integer(x - y)) }) })

      case Modulo =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Integer(x % y)) }) })

      case And =>
        More({ case ICFP.Boolean(x) => More({ case ICFP.Boolean(y) => Final(ICFP.Boolean(x && y)) }) })

      case Concatenate =>
        More({ case ICFP.String(x) => More({ case ICFP.String(y) => Final(ICFP.String(x + y)) }) })

      case IsGreaterThan =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Boolean(x > y)) }) })

      case Divide =>
        More({ case ICFP.Integer(x) => More({ case ICFP.Integer(y) => Final(ICFP.Integer(x / y)) }) })

      case If =>
        More({
          case ICFP.Boolean(true)  => More({ case atom => Final(atom) })
          case ICFP.Boolean(false) => More({ case atom => Final(atom) }, skip = 1)
        })

      case Negate =>
        More({ case ICFP.Integer(value) => Final(ICFP.Integer(-value)) })

      case LambdaAbstraction(variableNumber) =>
        ???

      case Not =>
        More({ case ICFP.Boolean(value) => Final(ICFP.Boolean(!value)) })

      case IntToString =>
        More({
          case int @ ICFP.Integer(value) if value >= 0 =>
            Final(ICFP.String(ICFP.String.fromHuman(int.toString.stripPrefix("I"))))
        })

      case StringToInt =>
        More({ case str: ICFP.String => Final(ICFP.Integer(ICFP.Integer.fromBase94(str.toString().stripPrefix("S")))) })
    }
  }

  def evaluate(expression: ICFP): ICFP.Atom = {
    val operators = mutable.Stack.empty[More]
    val expressions = mutable.Stack.empty[ICFP]
    val operandsInStack = mutable.Stack.empty[Int]
    expressions.push(expression)
    operators.push(More({ case value => Final(value) }))
    operandsInStack.push(1)

    while (operators.nonEmpty) {
      println("operators:")
      println(operators)
      println("expressions:")
      println(expressions)

      val More(f, skip) = operators.top

      if (expressions.top.isAtom) {
        operators.pop()
        val operands = operandsInStack.pop()
        (0 until skip).foreach(_ => expressions.pop())
        val expr = expressions.pop().asInstanceOf[ICFP.Atom]
        f(expr) match {
          case Final(res) =>
            (1 until (operands - skip)).foreach(_ => expressions.pop())
            expressions.push(res)

          case more: More =>
            operators.push(more)
            operandsInStack.push(operands - skip - 1)
        }
      } else {
        expressions.pop() match {
          case Unary(operator, expr) =>
            operators.push(opResult(operator))
            expressions.push(expr)
            operandsInStack.push(1)

          case Binary(operator, lhs, rhs) =>
            operators.push(opResult(operator))
            expressions.push(rhs)
            expressions.push(lhs)
            operandsInStack.push(2)

          case Ternary(operator, expr1, expr2, expr3) =>
            operators.push(opResult(operator))
            expressions.push(expr3)
            expressions.push(expr2)
            expressions.push(expr1)
            operandsInStack.push(3)

          case Variable(value) =>
            ???

          case other =>
            throw error(other)
        }
      }
    }

    if (expressions.length != 1 || !expressions.top.isInstanceOf[ICFP.Atom])
      throw error(expression)

    expressions.top.asInstanceOf[ICFP.Atom]
  }
}
