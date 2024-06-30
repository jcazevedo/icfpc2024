package net.jcazevedo.icfpc2024.`3d`

case class Board(cells: Vector[Vector[Cell]], t: Int) {
  def asString: String = {
    val columnLengths: Vector[Int] = {
      val columns = cells(0).length
      val columnLengths = Array.fill(columns)(0)
      cells.foreach(line =>
        line.zipWithIndex.foreach({
          case (Cell.Empty, col) =>
            columnLengths(col) = math.max(columnLengths(col), 1)
          case (Cell.Integer(value), col) =>
            columnLengths(col) = math.max(columnLengths(col), value.toString().length())
          case (Cell.Operator(value), col) =>
            columnLengths(col) = math.max(columnLengths(col), 1)
        })
      )
      columnLengths.toVector
    }

    def pad(value: String, col: Int): String =
      value.reverse.padTo(columnLengths(col), ' ').reverse

    val cellStr = cells
      .map(line =>
        line.zipWithIndex
          .map({
            case (Cell.Empty, col)           => pad(".", col)
            case (Cell.Integer(value), col)  => pad(value.toString, col)
            case (Cell.Operator(value), col) => pad(s"$value", col)
          })
          .mkString(" ")
      )
      .mkString("\n")

    s"[t=$t, x=?, y=?]\n$cellStr\n"
  }
}
