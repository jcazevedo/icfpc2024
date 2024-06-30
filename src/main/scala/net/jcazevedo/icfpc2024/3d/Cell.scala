package net.jcazevedo.icfpc2024.`3d`

sealed trait Cell

object Cell {
  case object Empty extends Cell
  case class Integer(value: BigInt) extends Cell
  case class Operator(op: Char) extends Cell
}
