package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec


object Day01 extends AoCApp {

    val dirs = Vector('N', 'O', 'S', 'W')
    def rotate(dir: Char, rot: Char): Char = {
        val rotation = if (rot == 'R') 1 else -1
        dirs((dirs.indexOf(dir) + rotation + dirs.length) % dirs.length)
    }

    val instructions: Seq[(Char, Int)] = inputText.split(',').map(_.trim.splitAt(1)).map {
        case (rot, steps) => (rot.head, steps.toInt)
    }


    val (_, locations) = instructions.foldLeft(('N', Vector((0, 0)))) {
        case ((lastDir, instr), (rot, steps)) =>
            val (lastX, lastY) = instr.last
            val newDir = rotate(lastDir, rot)

            val newLoc = newDir match {
                case 'N' => (lastX, lastY + steps)
                case 'O' => (lastX + steps, lastY)
                case 'S' => (lastX, lastY - steps)
                case 'W' => (lastX - steps, lastY)
            }

            (newDir, instr :+ newLoc)
    }

    def manhattenDistance(from: (Int, Int) = (0, 0), to: (Int, Int)): Int = {
        val (fromX, fromY) = from
        val (toX, toY) = to

        Math.abs(toX - fromX) + Math.abs(toY - fromY)
    }

    def findFirstDuplicate[T](input: Seq[T]): T = {

        val isDuplicated = input.groupBy(identity).mapValues(_.length > 1)

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
