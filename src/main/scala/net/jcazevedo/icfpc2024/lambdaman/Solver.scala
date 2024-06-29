package net.jcazevedo.icfpc2024.lambdaman

import scala.collection.mutable

object Solver {
  final val Steps = Map(
    "U" -> (-1, 0),
    "D" -> (1, 0),
    "L" -> (0, -1),
    "R" -> (0, 1)
  )

  def solve(grid: Vector[String]): String = {
    val Rows = grid.size
    val Cols = grid(0).size

    var startRow = -1
    var startCol = -1
    val toVisit = mutable.Set.empty[(Int, Int)]

    grid.zipWithIndex.foreach({ case (line, row) =>
      line.zipWithIndex.foreach({ case (ch, col) =>
        if (ch == 'L') {
          startRow = row
          startCol = col
        } else if (ch == '.') {
          toVisit.addOne((row, col))
        }
      })
    })

    def bfs(start: (Int, Int)): Map[(Int, Int), String] = {
      val visited = mutable.Map.empty[(Int, Int), String]
      visited(start) = ""

      val q = mutable.Queue.empty[(Int, Int)]
      q.enqueue(start)

      while (q.nonEmpty) {
        val (row, col) = q.dequeue()

        Steps.foreach({ case (op, (dr, dc)) =>
          val nr = row + dr
          var nc = col + dc
          val next = (nr, nc)

          if (nr >= 0 && nr < Rows && nc >= 0 && nc < Cols && grid(nr)(nc) != '#' && !visited.contains(next)) {
            visited(next) = visited((row, col)) + op
            q.enqueue(next)
          }
        })
      }

      visited.toMap
    }

    var ans = ""
    var curr = (startRow, startCol)

    while (toVisit.nonEmpty) {
      val dists = bfs(curr)
      val (next, ops) = dists.filter(v => toVisit.contains(v._1)).minBy(_._2.length)
      toVisit.remove(next)
      ans += ops
      curr = next
    }

    ans
  }
}
