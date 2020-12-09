package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.generated.Input2019
import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.year2019.Day02.{ProgramState, RequiredMoreInput, SuccessfullyCompleted, initProgram, parseProgram}

import scala.annotation.tailrec

object Day11 extends AoCApp {

    sealed abstract class Direction(val x: Int, val y: Int) {
        lazy val left: Direction = {
            this match {
                case Up => Left
                case Right => Up
                case Down => Right
                case Left => Down
            }
        }
        lazy val right: Direction = {
            this match {
                case Up => Right
                case Right => Down
                case Down => Left
                case Left => Up
            }
        }
        def unapply(value: Long): Option[Direction] = value match {
            case 0 => Some(left)
            case 1 => Some(right)
        }
    }
    case object Up extends Direction(0, 1)
    case object Right extends Direction(1, 0)
    case object Down extends Direction(0, -1)
    case object Left extends Direction(-1, 0)

    sealed trait Color {
        def value: Long = {
            this match {
                case White => 1
                case Black => 0
            }
        }
    }
    object Color {
        def apply(value: Long): Color = value match {
            case 0 => Black
            case 1 => White
        }
        def unapply(value: Long): Option[Color] = Some(apply(value))
    }
    case object White extends Color
    case object Black extends Color

    case class Robot(x: Int, y: Int, dir: Direction, program: ProgramState) {
        val pos: (Int, Int) = (x, y)
    }

    val program = parseProgram(Input2019.Day11)
    val startState = initProgram(Day09.instructions, program, Nil)
    val robot = Robot(0, 0, Up, startState)
    val blackStartField = paintField(robot, Map.empty)

    //printField(blackStartField)

    part(1, blackStartField.size)

    val whiteStartField = paintField(robot, Map((0, 0) -> White))

    part(2, {
        println()
        printField(whiteStartField)
    })

    def printField(field: Map[(Int, Int), Color]): String = {
        val (minX, maxX) = minMax(field.keys.map(_._1).iterator, Int.MaxValue, Int.MinValue)
        val (minY, maxY) = minMax(field.keys.map(_._2).iterator, Int.MaxValue, Int.MinValue)

        val out = for (y <- maxY to minY by -1) yield {
            val line = for (x <- minX to maxX) yield {
                field.get((x, y)) match {
                    case Some(White) => '#'
                    case _ => ' '
                }
            }
            s"${line.mkString("")}\n"
        }
        out.mkString(start = "\n", sep = "", end = "\n")
    }

    @tailrec
    def paintField(robot: Robot, field: Map[(Int, Int), Color]): Map[(Int, Int), Color] = {
        val color = field.getOrElse(robot.pos, Black)

        val newState = Day02.runProgram(robot.program.continue(input = color.value :: Nil, output = Nil))

        newState.status match {
            case SuccessfullyCompleted => field
            case RequiredMoreInput =>
                val Color(color) :: robot.dir(dir) :: Nil = newState.output
                paintField(Robot(robot.x + dir.x, robot.y + dir.y, dir, newState), field.updated(robot.pos, color))
            case _ => throw new Exception("should not be reached")
        }
    }

    @tailrec
    def minMax[T: Ordering](elems: Iterator[T], min: T, max: T): (T, T) = {
        if (!elems.hasNext) (min, max)
        else {
            val elem = elems.next()
            val ordering = implicitly[Ordering[T]]
            val newMin =
                if (ordering.lt(elem, min)) elem
                else min
            val newMax =
                if (ordering.gt(elem, max)) elem
                else max
            minMax(elems, newMin, newMax)
        }
    }

}
