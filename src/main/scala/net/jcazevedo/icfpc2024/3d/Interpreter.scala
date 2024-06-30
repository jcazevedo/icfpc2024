package net.jcazevedo.icfpc2024.`3d`

object Interpreter {
  def parse(grid: Vector[String]): Board = {
    Board(
      grid.map(line =>
        line
          .split(" ")
          .map({
            case "."                               => Cell.Empty
            case str if str.toLongOption.isDefined => Cell.Integer(str.toLong)
            case other                             => Cell.Operator(other(0))
          })
          .toVector
      ),
      1
    )
  }
}
