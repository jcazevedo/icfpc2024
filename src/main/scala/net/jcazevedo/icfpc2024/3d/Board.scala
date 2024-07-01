package net.jcazevedo.icfpc2024.`3d`

case class Board(values: Map[(Int, Int), Cell]) {
  lazy val minX = values.keySet.map(_._1).min
  lazy val maxX = values.keySet.map(_._1).max
  lazy val minY = values.keySet.map(_._2).min
  lazy val maxY = values.keySet.map(_._2).max

  def asString: String = {
    val columnLengths: Vector[Int] = {
      val columns = maxX - minX + 1
      val columnLengths = Array.fill(columns)(1)
      values.foreach({
        case ((x, y), Cell.Integer(value)) =>
          columnLengths(x - minX) = math.max(columnLengths(x - minX), value.toString.length())
        case ((x, y), Cell.Operator(_)) =>
          columnLengths(x - minX) = math.max(columnLengths(x - minX), 1)
      })
      columnLengths.toVector
    }

    def pad(value: String, col: Int): String =
      value.reverse.padTo(columnLengths(col), ' ').reverse

    (minY to maxY)
      .map(y =>
        (minX to maxX)
          .map(x =>
            values.get((x, y)) match {
              case None                       => pad(".", x - minX)
              case Some(Cell.Integer(value))  => pad(value.toString, x - minX)
              case Some(Cell.Operator(value)) => pad(s"$value", x - minX)
            }
          )
          .mkString(" ")
      )
      .mkString("\n")
  }
}
