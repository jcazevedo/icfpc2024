package net.jcazevedo.icfpc2024.lang

import scala.collection.mutable

import net.jcazevedo.icfpc2024.lang.ICFP.Expression.{Binary, Ternary, Unary}
import net.jcazevedo.icfpc2024.lang.ICFP.Operator
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Binary._
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Ternary.If
import net.jcazevedo.icfpc2024.lang.ICFP.Operator.Unary._

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
            Final(ICFP.String(ICFP.String.fromHuman(int.asString.stripPrefix("I"))))
        })

      case StringToInt =>
        More({ case str: ICFP.String => Final(ICFP.Integer(ICFP.Integer.fromBase94(str.asString.stripPrefix("S")))) })

      case other =>
        throw error(other)
    }
  }

  def betaReduction(icfp: ICFP, variable: Int, replacement: ICFP, isBound: Boolean = false): ICFP = {
    icfp match {
      case atom: ICFP.Atom =>
        atom
      case ICFP.Variable(v) if v == variable && !isBound =>
        replacement
      case ICFP.Variable(v) =>
        ICFP.Variable(v)
      case ICFP.Expression.Unary(LambdaAbstraction(value), expression) =>
        ICFP.Expression.Unary(
          LambdaAbstraction(value),
          betaReduction(expression, variable, replacement, isBound || value == variable)
        )
      case ICFP.Expression.Unary(op, exp) =>
        ICFP.Expression.Unary(op, betaReduction(exp, variable, replacement, isBound))
      case ICFP.Expression.Binary(op, lhs, rhs) =>
        ICFP.Expression.Binary(
          op,
          betaReduction(lhs, variable, replacement, isBound),
          betaReduction(rhs, variable, replacement, isBound)
        )
      case ICFP.Expression.Ternary(op, e1, e2, e3) =>
        ICFP.Expression.Ternary(
          op,
          betaReduction(e1, variable, replacement, isBound),
          betaReduction(e2, variable, replacement, isBound),
          betaReduction(e3, variable, replacement, isBound)
        )
    }
  }

  def freeVariables(icfp: ICFP, bound: Set[Int] = Set.empty): Set[Int] = {
    icfp match {
      case atom: ICFP.Atom =>
        Set.empty
      case ICFP.Variable(v) =>
        if (!bound.contains(v)) Set(v) else Set.empty
      case ICFP.Expression.Unary(LambdaAbstraction(value), expression) =>
        freeVariables(expression, bound + value)
      case ICFP.Expression.Unary(_, exp) =>
        freeVariables(exp, bound)
      case ICFP.Expression.Binary(op, lhs, rhs) =>
        freeVariables(lhs, bound) ++ freeVariables(rhs, bound)
      case ICFP.Expression.Ternary(op, e1, e2, e3) =>
        freeVariables(e1, bound) ++ freeVariables(e2, bound) ++ freeVariables(e3, bound)
    }
  }

  def evaluate(expression: ICFP): ICFP.Atom = {
    val operations = mutable.Stack.empty[More]
    val expressions = mutable.Stack.empty[ICFP]
    val operandsInStack = mutable.Stack.empty[Int]
    val bindings = mutable.Stack.empty[ICFP]

    expressions.push(expression)
    operations.push(More({ case value => Final(value) }))
    operandsInStack.push(1)

    while (operations.nonEmpty) {
      val More(f, skip) = operations.top

      if (skip > 0) {
        (0 until skip).foreach(_ => expressions.pop())
        operations.pop()
        operations.push(More(f, 0))
        val operands = operandsInStack.pop()
        operandsInStack.push(operands - skip)

      } else if (expressions.top.isAtom) {
        operations.pop()
        val operands = operandsInStack.pop()
        val expr = expressions.pop()
        f(expr.asInstanceOf[ICFP.Atom]) match {
          case Final(res) =>
            (1 until operands).foreach(_ => expressions.pop())
            expressions.push(res)

          case more: More =>
            operations.push(more)
            operandsInStack.push(operands - 1)
        }

      } else {
        expressions.pop() match {
          // Lambda absractions have a special treatment.
          case Unary(LambdaAbstraction(variable), expr) =>
            val free = freeVariables(bindings.top)
            val (finalVariable, finalExpr) = if (free.contains(variable)) {
              // If variable is in the free variables of the substitution, we replace it by something else.
              val exprFree = freeVariables(expr)
              val nextVariable = Iterator.iterate(0)(_ + 1).find(v => !exprFree.contains(v)).get
              (nextVariable, betaReduction(expr, variable, ICFP.Variable(nextVariable)))
            } else (variable, expr)
            operations.push(More({ case atom => Final(atom) }))
            expressions.push(betaReduction(finalExpr, finalVariable, bindings.pop()))
            operandsInStack.push(1)

          case Unary(operator, expr) =>
            operations.push(opResult(operator))
            expressions.push(expr)
            operandsInStack.push(1)

          // Lambda applications have a special treatment.
          case Binary(LambdaApplication, lhs, rhs) =>
            bindings.push(rhs)
            operations.push(More({ case atom => Final(atom) }))
            expressions.push(lhs)
            operandsInStack.push(1)

          case Binary(operator, lhs, rhs) =>
            operations.push(opResult(operator))
            expressions.push(rhs)
            expressions.push(lhs)
            operandsInStack.push(2)

          case Ternary(operator, expr1, expr2, expr3) =>
            operations.push(opResult(operator))
            expressions.push(expr3)
            expressions.push(expr2)
            expressions.push(expr1)
            operandsInStack.push(3)

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
