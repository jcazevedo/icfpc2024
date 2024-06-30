package net.jcazevedo.icfpc2024.spaceship

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

  def lineSteps(from: Int, to: Int): Vector[Int] = {
    val dist = math.abs(to - from)
    val diff = math.signum(to - from)
    val t = math.sqrt(dist.toDouble).toInt
    val tmpAccelerations = Vector.fill(math.max(t, 1))(diff) ++ Vector.fill(math.max(t, 1))(-diff)
    val finalDist = math.max(t, 1) * math.max(t, 1)
    if (finalDist == dist) tmpAccelerations
    else {
      val finalAcc = tmpAccelerations.last
      tmpAccelerations.init ++ Vector.fill(dist - finalDist)(0) :+ finalAcc
    }
  }

  def steps(from: (Int, Int), to: (Int, Int)): String = {
    val stepsX = lineSteps(from._1, to._1)
    val stepsY = lineSteps(from._2, to._2)
    stepsX.zipAll(stepsY, 0, 0).map(ToStep).mkString
  }

  def solve(points: Vector[(Int, Int)]): String = {
    val toVisit = points.toSet
    var ans = ""
    var curr = (0, 0)

    toVisit.foreach { next =>
      ans += steps(curr, next)
      curr = next
    }

    ans
  }
}
