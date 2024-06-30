package net.jcazevedo.icfpc2024.lang

sealed trait ICFP {
  def isAtom: Boolean

  def asString: String =
    this match {
      case ICFP.Boolean(value) =>
        if (value) "T" else "F"

      case ICFP.Integer(value) =>
        if (value < 0) ICFP.Expression.Unary(ICFP.Operator.Unary.Negate, ICFP.Integer(-value)).asString
        else s"I${ICFP.Integer.toBase94(value)}"

      case ICFP.String(value) =>
        s"S${value.map(ch => (ICFP.String.Order.indexOf(ch.toInt) + 33).toChar).mkString}"

      case ICFP.Variable(value) =>
        s"v${ICFP.Integer.toBase94(value)}"

      case ICFP.Expression.Unary(operator, expression) =>
        s"${operator.asString} ${expression.asString}"

      case ICFP.Expression.Binary(operator, lhs, rhs) =>
        s"${operator.asString} ${lhs.asString} ${rhs.asString}"

      case ICFP.Expression.Ternary(operator, expr1, expr2, expr3) =>
        s"${operator.asString} ${expr1.asString} ${expr2.asString} ${expr3.asString}"
    }
}

object ICFP {
  sealed trait Atom extends ICFP {
    def result: java.lang.String

    def isAtom: scala.Boolean =
      true
  }

  case class Boolean(value: scala.Boolean) extends ICFP.Atom {
    def result = value.toString
  }

  case class Integer(value: BigInt) extends ICFP.Atom {
    def result = s"$value"
  }

  object Integer {
    def toBase94(value: BigInt): java.lang.String =
      if (value == 0)
        s"${(value + 33).toChar}"
      else {
        var ans = ""
        var curr = value
        while (curr > 0) {
          ans = s"${(curr % 94 + 33).toChar}$ans"
          curr = curr / 94
        }
        ans
      }

    def fromBase94(digits: java.lang.String): BigInt =
      digits.foldLeft(BigInt(0))((ans, digit) => ans * 94 + (digit.toInt - 33))
  }

  case class String(value: java.lang.String) extends ICFP.Atom {
    def result = value
  }

  object String {
    final val Order: java.lang.String =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

    def fromHuman(str: java.lang.String): java.lang.String =
      str.map(ch => Order(ch.toInt - 33)).mkString
  }

  case class Variable(value: Int) extends ICFP {
    def isAtom: scala.Boolean =
      false
  }

  sealed trait Operator {
    def asString: java.lang.String =
      this match {
        case Operator.Unary.Negate                            => "U-"
        case Operator.Unary.Not                               => "U!"
        case Operator.Unary.StringToInt                       => "U#"
        case Operator.Unary.IntToString                       => "U$"
        case Operator.Unary.LambdaAbstraction(variableNumber) => s"L${ICFP.Integer.toBase94(variableNumber)}"
        case Operator.Binary.Add                              => "B+"
        case Operator.Binary.Subtract                         => "B-"
        case Operator.Binary.Multiply                         => "B*"
        case Operator.Binary.Divide                           => "B/"
        case Operator.Binary.Modulo                           => "B%"
        case Operator.Binary.IsLessThan                       => "B<"
        case Operator.Binary.IsGreaterThan                    => "B>"
        case Operator.Binary.IsEqual                          => "B="
        case Operator.Binary.Or                               => "B|"
        case Operator.Binary.And                              => "B&"
        case Operator.Binary.Concatenate                      => "B."
        case Operator.Binary.Take                             => "BT"
        case Operator.Binary.Drop                             => "BD"
        case Operator.Binary.LambdaApplication                => "B$"
        case Operator.Ternary.If                              => "?"
      }
  }

  object Operator {
    sealed trait Unary extends Operator

    object Unary {
      case object Negate extends ICFP.Operator.Unary
      case object Not extends ICFP.Operator.Unary
      case object StringToInt extends ICFP.Operator.Unary
      case object IntToString extends ICFP.Operator.Unary
      case class LambdaAbstraction(variableNumber: Int) extends ICFP.Operator.Unary
    }

    sealed trait Binary extends Operator

    object Binary {
      case object Add extends ICFP.Operator.Binary
      case object Subtract extends ICFP.Operator.Binary
      case object Multiply extends ICFP.Operator.Binary
      case object Divide extends ICFP.Operator.Binary
      case object Modulo extends ICFP.Operator.Binary
      case object IsLessThan extends ICFP.Operator.Binary
      case object IsGreaterThan extends ICFP.Operator.Binary
      case object IsEqual extends ICFP.Operator.Binary
      case object Or extends ICFP.Operator.Binary
      case object And extends ICFP.Operator.Binary
      case object Concatenate extends ICFP.Operator.Binary
      case object Take extends ICFP.Operator.Binary
      case object Drop extends ICFP.Operator.Binary
      case object LambdaApplication extends ICFP.Operator.Binary
    }

    sealed trait Ternary extends Operator

    case object Ternary {
      case object If extends ICFP.Operator.Ternary
    }
  }

  sealed trait Expression extends ICFP {
    def isAtom: scala.Boolean = false
  }

  object Expression {
    case class Unary(operator: ICFP.Operator.Unary, expression: ICFP) extends Expression
    case class Binary(operator: ICFP.Operator.Binary, lhs: ICFP, rhs: ICFP) extends Expression
    case class Ternary(operator: ICFP.Operator.Ternary, expr1: ICFP, expr2: ICFP, expr3: ICFP) extends Expression
  }
}
