package net.jcazevedo.icfpc2024.`3d`

object Interpreter {
  def parse(grid: Vector[String]): Board =
    Board(
      grid.zipWithIndex
        .flatMap({ case (line, y) =>
          line
            .split(" ")
            .zipWithIndex
            .flatMap({ case (ch, x) =>
              ch match {
                case "."                               => None
                case str if str.toLongOption.isDefined => Some((x, y) -> Cell.Integer(str.toLong))
                case other                             => Some((x, y) -> Cell.Operator(other(0)))
              }
            })
        })
        .toMap
    )
}
