package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.generated.Input2016
import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day01 extends AoCApp {

    val instructions: Seq[(Rotation, Int)] = splitInput(Input2016.Day01, ',').map(_.trim.splitAt(1)).map {
        case ("R", steps) => ((rotateRight _).tupled, steps.toInt)
        case ("L", steps) => ((rotateLeft _).tupled, steps.toInt)
        case _ => throw new Exception("Unknown instruction!")
    }

    type Rotation = ((Int, Int)) => (Int, Int)
    def add(l: (Int, Int), r: (Int, Int)): (Int, Int) = (l._1 + r._1, l._2 + r._2)
    def scale(pos: (Int, Int), scale: Int): (Int, Int) = (pos._1 * scale, pos._2 * scale)
    def rotateLeft(x: Int, y: Int): (Int, Int) = (-y, x)
    def rotateRight(x: Int, y: Int): (Int, Int) = (y, -x)


    val (_, locations) = instructions.foldLeft(((0, 1), Vector((0, 0)))) {
        case ((direction, corners), (rot, steps)) =>
            val newDir = rot(direction)

            (newDir, corners :+ add(corners.last, scale(newDir, steps)))
    }

    def manhattenDistance(from: (Int, Int) = (0, 0), to: (Int, Int)): Int = {
        val (fromX, fromY) = from
        val (toX, toY) = to

        Math.abs(toX - fromX) + Math.abs(toY - fromY)
    }

    def findFirstDuplicate[T](input: Seq[T]): T = {

        val isDuplicated = input.groupBy(identity).view.mapValues(_.length > 1)

        @tailrec
        def firstDup(input: Seq[T]): T = {
            if (input.length == 1) input.head
            else if (isDuplicated(input.head)) input.head
            else firstDup(input.tail)
        }

        firstDup(input)
    }

    part(1, manhattenDistance(to = locations.last))

    val interpolatedLocations = locations.head :: locations.sliding(2).flatMap {way =>
        def step(x: Int, y: Int) = if (y - x > 0) 1 else -1
        def interpolate(x: Int, y: Int) = {
            val s = step(x, y)
            x + s to y by s
        }

        val Vector((fromX, fromY), (toX, toY)) = way
        if (fromX == toX) interpolate(fromY, toY).map((fromX, _))
        else interpolate(fromX, toX).map((_, fromY))
    }.toList

    part(2, manhattenDistance(to = findFirstDuplicate(interpolatedLocations)))

}
