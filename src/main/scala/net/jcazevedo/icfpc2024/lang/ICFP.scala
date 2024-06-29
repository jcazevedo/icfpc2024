package net.jcazevedo.icfpc2024.lang

sealed trait ICFP {
  def isAtom: Boolean
}

object ICFP {
  sealed trait Atom extends ICFP {
    def result: java.lang.String

    def isAtom: scala.Boolean =
      true
  }

  case class Boolean(value: scala.Boolean) extends ICFP.Atom {
    override def toString = if (value) "T" else "F"
    def result = value.toString
  }

  case class Integer(value: Long) extends ICFP.Atom {
    override def toString = {
      if (value < 0)
        ICFP.Expression.Unary(ICFP.Operator.Unary.Negate, ICFP.Integer(-value)).toString()
      else
        s"I${Integer.toBase94(value)}"
    }

    def result = s"$value"
  }

  object Integer {
    def toBase94(value: Long): java.lang.String =
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

    def fromBase94(digits: java.lang.String): Long =
      digits.foldLeft(0L)((ans, digit) => ans * 94 + (digit.toInt - 33))
  }

  case class String(value: java.lang.String) extends ICFP.Atom {
    override def toString =
      s"S${value.map(ch => (String.Order.indexOf(ch.toInt) + 33).toChar).mkString}"

    def result = value
  }

  object String {
    final val Order: java.lang.String =
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"

    def fromHuman(str: java.lang.String): java.lang.String =
      str.map(ch => Order(ch.toInt - 33)).mkString
  }

  case class Variable(value: Long) extends ICFP {
    override def toString =
      s"v${Integer.toBase94(value)}"

    def isAtom: scala.Boolean =
      false
  }

  sealed trait Operator

  object Operator {
    sealed trait Unary extends Operator

    object Unary {
      case object Negate extends ICFP.Operator.Unary {
        override def toString = "U-"
      }
      case object Not extends ICFP.Operator.Unary {
        override def toString = "U!"
      }
      case object StringToInt extends ICFP.Operator.Unary {
        override def toString = "U#"
      }
      case object IntToString extends ICFP.Operator.Unary {
        override def toString = "U$"
      }
      case class LambdaAbstraction(variableNumber: Long) extends ICFP.Operator.Unary {
        override def toString = s"L${ICFP.Integer.toBase94(variableNumber)}"
      }
    }

    sealed trait Binary extends Operator

    object Binary {
      case object Add extends ICFP.Operator.Binary {
        override def toString = "B+"
      }
      case object Subtract extends ICFP.Operator.Binary {
        override def toString = "B-"
      }
      case object Multiply extends ICFP.Operator.Binary {
        override def toString = "B*"
      }
      case object Divide extends ICFP.Operator.Binary {
        override def toString = "B/"
      }
      case object Modulo extends ICFP.Operator.Binary {
        override def toString = "B%"
      }
      case object IsLessThan extends ICFP.Operator.Binary {
        override def toString = "B<"
      }
      case object IsGreaterThan extends ICFP.Operator.Binary {
        override def toString = "B>"
      }
      case object IsEqual extends ICFP.Operator.Binary {
        override def toString = "B="
      }
      case object Or extends ICFP.Operator.Binary {
        override def toString = "B|"
      }
      case object And extends ICFP.Operator.Binary {
        override def toString = "B&"
      }
      case object Concatenate extends ICFP.Operator.Binary {
        override def toString = "B."
      }
      case object Take extends ICFP.Operator.Binary {
        override def toString = "BT"
      }
      case object Drop extends ICFP.Operator.Binary {
        override def toString = "BD"
      }
      case object LambdaApplication extends ICFP.Operator.Binary {
        override def toString = "B$"
      }
    }

    sealed trait Ternary extends Operator

    case object Ternary {
      case object If extends ICFP.Operator.Ternary {
        override def toString = "?"
      }
    }
  }

  sealed trait Expression extends ICFP {
    def isAtom: scala.Boolean =
      false
  }

  object Expression {
    case class Unary(operator: ICFP.Operator.Unary, expression: ICFP) extends Expression {
      override def toString = s"$operator $expression"
    }
    case class Binary(operator: ICFP.Operator.Binary, lhs: ICFP, rhs: ICFP) extends Expression {
      override def toString = s"$operator $lhs $rhs"
    }
    case class Ternary(operator: ICFP.Operator.Ternary, expr1: ICFP, expr2: ICFP, expr3: ICFP) extends Expression {
      override def toString = s"$operator $expr1 $expr2 $expr3"
    }
  }
}
