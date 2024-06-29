package net.jcazevedo.icfpc2024.spaceship

import scala.collection.mutable

object Solver {
  final val Steps: Map[String, (Int, Int)] = Map(
    "1" -> (-1, -1),
    "2" -> (0, -1),
    "3" -> (1, -1),
    "4" -> (-1, 0),
    "5" -> (0, 0),
    "6" -> (1, 0),
    "7" -> (-1, 1),
    "8" -> (0, 1),
    "9" -> (1, 1)
  )

  final val ToStep: Map[(Int, Int), String] =
    Steps.map({ case (k, v) => v -> k })

  case class State(px: Int, py: Int, vx: Int, vy: Int)

  def solve(points: Vector[(Int, Int)]): String = {
    def bfs(from: (Int, Int), to: (Int, Int)): String = {
      // X
      val visitedX = mutable.Map.empty[(Int, Int), Vector[Int]]
      visitedX((from._1, 0)) = Vector.empty
      val q = mutable.Queue.empty[(Int, Int)]
      q.enqueue((from._1, 0))

      while (q.nonEmpty && !visitedX.contains((to._1, 0))) {
        val curr @ (currX, currVX) = q.dequeue()

        (-1 to 1).foreach { dvx =>
          val next = (currX + currVX + dvx, currVX + dvx)
          if (!visitedX.contains(next)) {
            visitedX(next) = visitedX(curr) :+ dvx
            q.enqueue(next)
          }
        }
      }

      // Y
      val visitedY = mutable.Map.empty[(Int, Int), Vector[Int]]
      visitedY((from._2, 0)) = Vector.empty
      q.clear()
      q.enqueue((from._2, 0))

      while (q.nonEmpty && !visitedY.contains((to._2, 0))) {
        val curr @ (currY, currVY) = q.dequeue()

        (-1 to 1).foreach { dvy =>
          val next = (currY + currVY + dvy, currVY + dvy)
          if (!visitedY.contains(next)) {
            visitedY(next) = visitedY(curr) :+ dvy
            q.enqueue(next)
          }
        }
      }

      // Combine
      val dxs = visitedX((to._1, 0))
      val dys = visitedY((to._2, 0))
      val ds = dxs.zipAll(dys, 0, 0)
      ds.map(ToStep).mkString
    }

    var ans = ""
    var curr = (0, 0)

    points.foreach(next => {
      val ops = bfs(curr, next)
      ans += ops
      curr = next
    })

    ans
  }
}
