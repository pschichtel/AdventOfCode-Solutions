package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.math._

object Day06 extends AoCApp {

    private val Command = "(turn off|turn on|toggle) (\\d+),(\\d+) through (\\d+),(\\d+)".r

    def toCommands[T](inputs: Seq[String])(cm: String => T => T): Seq[(T => T, (Int, Int), (Int, Int))] = inputs.map {
        case Command(command, x1, y1, x2, y2) =>
            (cm(command), (x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
    }

    def applyCommands[T](grid: Array[T], size: Int, commands: Seq[(T => T, (Int, Int), (Int, Int))]): Array[T] = {
        commands.foldLeft(grid) {
            case (field, (command, (x1, y1), (x2, y2))) =>
                for (x <- x1 to x2) {
                    for (y <- y1 to y2) {
                        val pos = y * size + x
                        field(pos) = command(field(pos))
                    }
                }
                field
        }
    }

    override def solution: (Any, Any) = {
        val input = asLines(Input2015.Day06)

        val commands1 = toCommands(input) {
            case "turn on" => (_: Boolean) => true
            case "turn off" => (_: Boolean) => false
            case "toggle" => (s: Boolean) => !s
        }

        val field1 = applyCommands(Array.ofDim[Boolean](1000 * 1000), 1000, commands1)

        val part1 = field1.count(_ == true)

        val commands2 = toCommands(input) {
            case "turn on" => (s: Int) => s + 1
            case "turn off" => (s: Int) => max(0, s - 1)
            case "toggle" => (s: Int) => s + 2
        }

        val field2 = applyCommands(Array.ofDim[Int](1000 * 1000), 1000, commands2)

        val part2 = field2.sum

        (part1, part2)

    }
}
