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

  def error(operator: ICFP.Operator): Exception =
    new IllegalArgumentException(s"Unexpected operator $operator")

  sealed trait Operation
  case class Final(res: ICFP.Atom) extends Operation
  case class More(f: PartialFunction[ICFP.Atom, Operation], skip: Int = 0) extends Operation

  def opResult(operator: Operator): More = {
    operator match {
      case Drop =>
        More({ case ICFP.Integer(x) => More({ case ICFP.String(y) => Final(ICFP.String(y.drop(x.toInt))) }) })

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

      case Not =>
        More({ case ICFP.Boolean(value) => Final(ICFP.Boolean(!value)) })

      case IntToString =>
        More({
          case int @ ICFP.Integer(value) if value >= 0 =>
            Final(ICFP.String(ICFP.String.fromHuman(int.toString.stripPrefix("I"))))
        })

      case StringToInt =>
        More({ case str: ICFP.String => Final(ICFP.Integer(ICFP.Integer.fromBase94(str.toString().stripPrefix("S")))) })

      case other =>
        throw error(other)
    }
  }

  case class ExpressionContext(expr: ICFP, bindings: List[ICFP], bound: Map[Long, ICFP])

  def evaluate(expression: ICFP): ICFP.Atom = {
    val operations = mutable.Stack.empty[More]
    val expressions = mutable.Stack.empty[ExpressionContext]
    val operandsInStack = mutable.Stack.empty[Int]
    expressions.push(ExpressionContext(expression, List.empty, Map.empty))
    operations.push(More({ case value => Final(value) }))
    operandsInStack.push(1)

    while (operations.nonEmpty) {
      // println("operators:")
      // println(operations)
      // println("expressions:")
      // println(expressions)
      // println("bindings:")
      // println(bindings)
      // println("context:")
      // println(context)

      val More(f, skip) = operations.top
      if (skip > 0) {
        (0 until skip).foreach(_ => expressions.pop())
        operations.pop()
        operations.push(More(f, 0))
        val operands = operandsInStack.pop()
        operandsInStack.push(operands - skip)
      } else if (expressions.top.expr.isAtom) {
        val More(f, _) = operations.pop()
        val operands = operandsInStack.pop()
        val expr = expressions.pop()
        f(expr.expr.asInstanceOf[ICFP.Atom]) match {
          case Final(res) =>
            (1 until operands).foreach(_ => expressions.pop())
            expressions.push(ExpressionContext(res, expr.bindings, expr.bound))

          case more: More =>
            operations.push(more)
            operandsInStack.push(operands - 1)
        }
      } else {
        expressions.pop() match {
          // Lambda absractions have a special treatment.
          case ExpressionContext(Unary(LambdaAbstraction(variable), expr), bindings, bound) =>
            operations.push(More({ case atom => Final(atom) }))
            expressions.push(ExpressionContext(expr, bindings.tail, bound + (variable -> bindings.head)))
            operandsInStack.push(1)

          case ExpressionContext(Unary(operator, expr), bindings, bound) =>
            operations.push(opResult(operator))
            expressions.push(ExpressionContext(expr, bindings, bound))
            operandsInStack.push(1)

          // Lambda applications have a special treatment.
          case ExpressionContext(Binary(LambdaApplication, lhs, rhs), bindings, bound) =>
            operations.push(More({ case atom => Final(atom) }))
            expressions.push(ExpressionContext(lhs, rhs :: bindings, bound))
            operandsInStack.push(1)

          case ExpressionContext(Binary(operator, lhs, rhs), bindings, bound) =>
            operations.push(opResult(operator))
            expressions.push(ExpressionContext(rhs, bindings, bound))
            expressions.push(ExpressionContext(lhs, bindings, bound))
            operandsInStack.push(2)

          case ExpressionContext(Ternary(operator, expr1, expr2, expr3), bindings, bound) =>
            operations.push(opResult(operator))
            expressions.push(ExpressionContext(expr3, bindings, bound))
            expressions.push(ExpressionContext(expr2, bindings, bound))
            expressions.push(ExpressionContext(expr1, bindings, bound))
            operandsInStack.push(3)

          case ExpressionContext(Variable(value), bindings, bound) =>
            println(s"Replacing ${Variable(value)} with ${bound(value)}")
            expressions.push(ExpressionContext(bound(value), bindings, bound))

          case other =>
            throw error(other.expr)
        }
      }
    }

    if (expressions.length != 1 || !expressions.top.expr.isInstanceOf[ICFP.Atom])
      throw error(expression)

    expressions.top.expr.asInstanceOf[ICFP.Atom]
  }
}
