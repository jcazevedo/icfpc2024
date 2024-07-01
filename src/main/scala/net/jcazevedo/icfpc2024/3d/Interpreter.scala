package net.jcazevedo.icfpc2024.`3d`

import scala.collection.mutable

import com.typesafe.scalalogging.LazyLogging

import net.jcazevedo.icfpc2024.`3d`.Interpreter.SingleReduction.TimeWarp

object Interpreter extends LazyLogging {
  def parse(grid: Vector[String]): Board =
    Board(
      grid.zipWithIndex
        .flatMap({ case (line, y) =>
          line
            .split("\\s+")
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

  sealed trait Reduction
  object Reduction {
    case object Crash extends Reduction
    case class Result(value: Option[BigInt]) extends Reduction
  }

  sealed trait SingleReduction
  object SingleReduction {
    case object Crash extends SingleReduction
    case class Result(value: Cell) extends SingleReduction
    case object Halt extends SingleReduction
    case class Next(board: Board) extends SingleReduction
    case class TimeWarp(dt: Int, mutations: Map[(Int, Int), Cell]) extends SingleReduction
  }

  def reduceOne(board: Board): SingleReduction = {
    val read = mutable.Set.empty[(Int, Int)]
    val nextMap = mutable.Map.empty[(Int, Int), Cell]

    def isSubmit(coords: (Int, Int)): Boolean =
      board.values.get((coords)).exists(_ == Cell.Operator('S'))

    def move(from: (Int, Int), to: (Int, Int)): Option[SingleReduction] = {
      board.values
        .get(from)
        .foreach(cell =>
          nextMap.updateWith(to)({
            case Some(cell) =>
              return Some(SingleReduction.Crash)
            case None =>
              read.add(from)
              if (isSubmit(to)) return Some(SingleReduction.Result(cell))
              Some(cell)
          })
        )
      None
    }

    def arithmetic(pos: (Int, Int), op: (BigInt, BigInt) => BigInt): Option[SingleReduction] = {
      val (x, y) = pos
      for {
        i1 <- board.values.get((x - 1, y))
        i2 <- board.values.get((x, y - 1))
      } (i1, i2) match {
        case (Cell.Integer(v1), Cell.Integer(v2)) =>
          read.add((x, y - 1))
          read.add((x - 1, y))
          val cell = Cell.Integer(op(v1, v2))
          nextMap.updateWith((x + 1, y))({
            case Some(_) =>
              return Some(SingleReduction.Crash)
            case None =>
              if (isSubmit((x + 1, y))) return Some(SingleReduction.Result(cell))
              Some(cell)
          })
          nextMap.updateWith((x, y + 1))({
            case Some(_) =>
              return Some(SingleReduction.Crash)
            case None =>
              if (isSubmit((x, y + 1))) return Some(SingleReduction.Result(cell))
              Some(cell)
          })
        case other =>
          return Some(SingleReduction.Crash)
      }
      None
    }

    def comparison(pos: (Int, Int), op: (Cell, Cell) => Boolean): Option[SingleReduction] = {
      val (x, y) = pos
      for {
        i1 <- board.values.get((x, y - 1))
        i2 <- board.values.get((x - 1, y))
        if op(i1, i2)
      } {
        read.add((x, y - 1))
        read.add((x - 1, y))
        nextMap.updateWith((x + 1, y))({
          case Some(_) =>
            return Some(SingleReduction.Crash)
          case None =>
            if (isSubmit((x + 1, y))) return Some(SingleReduction.Result(i1))
            Some(i1)
        })
        nextMap.updateWith((x, y + 1))({
          case Some(_) =>
            return Some(SingleReduction.Crash)
          case None =>
            if (isSubmit((x, y + 1))) return Some(SingleReduction.Result(i2))
            Some(i2)
        })
      }
      None
    }

    var timewarps: Option[TimeWarp] = None

    board.values.foreach({ case ((x, y), cell) =>
      cell match {
        case Cell.Operator('<') => move((x + 1, y), (x - 1, y)).foreach(x => return x)
        case Cell.Operator('>') => move((x - 1, y), (x + 1, y)).foreach(x => return x)
        case Cell.Operator('^') => move((x, y + 1), (x, y - 1)).foreach(x => return x)
        case Cell.Operator('v') => move((x, y - 1), (x, y + 1)).foreach(x => return x)
        case Cell.Operator('+') => arithmetic((x, y), _ + _).foreach(x => return x)
        case Cell.Operator('-') => arithmetic((x, y), _ - _).foreach(x => return x)
        case Cell.Operator('*') => arithmetic((x, y), _ * _).foreach(x => return x)
        case Cell.Operator('/') => arithmetic((x, y), _ / _).foreach(x => return x)
        case Cell.Operator('%') => arithmetic((x, y), _ % _).foreach(x => return x)
        case Cell.Operator('=') => comparison((x, y), _ == _).foreach(x => return x)
        case Cell.Operator('#') => comparison((x, y), _ != _).foreach(x => return x)
        case Cell.Operator('@') =>
          for {
            up <- board.values.get((x, y - 1))
            right <- board.values.get((x + 1, y))
            down <- board.values.get((x, y + 1))
            left <- board.values.get((x - 1, y))
          } (right, down, left) match {
            case (Cell.Integer(dy), Cell.Integer(dt), Cell.Integer(dx)) =>
              if (dt.toInt == 0)
                return SingleReduction.Crash

              read.add((x, y - 1))
              read.add((x + 1, y))
              read.add((x, y + 1))
              read.add((x - 1, y))

              val mutation = (x - dx.toInt, y - dy.toInt) -> up
              timewarps match {
                case None =>
                  timewarps = Some(TimeWarp(dt.toInt, Map(mutation)))
                case Some(TimeWarp(existingDt, existingMutations)) =>
                  if (existingDt != dt.toInt)
                    return SingleReduction.Crash
                  if (existingMutations.get(mutation._1).exists(_ != mutation._2))
                    return SingleReduction.Crash
                  timewarps = Some(TimeWarp(existingDt, existingMutations + mutation))
              }

            case other =>
              return SingleReduction.Crash
          }
        case _ => // Do nothing for other operators.
      }
    })

    timewarps.foreach(x => return x)

    board.values.foreach({ case ((x, y), cell) =>
      if (!read.contains((x, y)) && !nextMap.contains((x, y))) {
        nextMap((x, y)) = cell
      }
    })

    if (read.isEmpty)
      return SingleReduction.Halt

    SingleReduction.Next(Board(nextMap.toMap))
  }

  def reduce(board: Board, inputA: Option[BigInt], inputB: Option[BigInt], maxSteps: Option[Int] = None): Reduction = {
    val start = board.copy(board.values.map({
      case (coords, Cell.Operator('A')) =>
        coords -> inputA.fold[Cell](Cell.Operator('A'))(Cell.Integer)
      case (coords, Cell.Operator('B')) =>
        coords -> inputB.fold[Cell](Cell.Operator('B'))(Cell.Integer)
      case other => other
    }))

    val ticks = mutable.Map.empty[Int, Board]
    var curr = start
    var t = 1
    var minX = start.minX
    var maxX = start.maxX
    var minY = start.minY
    var maxY = start.maxY

    ticks(t) = curr

    logger.info(s"[t=$t, x=${maxX - minX + 1}, y=${maxY - minY + 1}]\n${curr.asString}")

    var steps = 0

    while (true) {
      reduceOne(curr) match {
        case SingleReduction.Crash =>
          return Reduction.Crash
        case SingleReduction.Result(Cell.Integer(value)) =>
          return Reduction.Result(Some(value))
        case SingleReduction.Result(_) =>
          return Reduction.Result(None)
        case SingleReduction.Halt =>
          return Reduction.Result(None)
        case SingleReduction.TimeWarp(dt, mutations) =>
          val diff = t - dt
          ticks(diff) = ticks(diff).copy(ticks(diff).values ++ mutations)
          curr = ticks(diff)
          t = diff
        case SingleReduction.Next(next) =>
          curr = next
          minX = math.min(minX, next.minX)
          maxX = math.max(maxX, next.maxX)
          minY = math.min(minY, next.minY)
          maxY = math.max(maxY, next.maxY)
          t += 1
          ticks(t) = curr
      }

      steps += 1
      logger.info(s"[t=$t, x=${maxX - minX + 1}, y=${maxY - minY + 1}, steps=$steps]\n${curr.asString}")

      maxSteps.foreach(maxS => if (steps >= maxS) return Reduction.Result(None))
    }

    Reduction.Result(None)
  }
}
