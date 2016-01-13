import scala.io.Source
import scala.math._

object Day6 extends AoCApp {

    println("Day  7")

    val Command = raw"(turn off|turn on|toggle) (\d+),(\d+) through (\d+),(\d+)".r
    val input = Source.fromFile("day6.txt").mkString.trim.split('\n').map(_.trim).toSeq

    def toCommands[T](inputs: Seq[String])(cm: String => (T => T)) = input.map {
        case Command(command, x1, y1, x2, y2) =>
            (cm(command), (x1.toInt, y1.toInt), (x2.toInt, y2.toInt))
    }

    def applyCommands[T](grid: Array[T], size: Int, commands: Seq[(T => T, (Int, Int), (Int, Int))]) = {
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

    val commands1 = toCommands(input) {
        case "turn on" => (s: Boolean) => true
        case "turn off" => (s: Boolean) => false
        case "toggle" => (s: Boolean) => !s
    }

    val field1 = applyCommands(Array.ofDim(1000 * 1000), 1000, commands1)

    println("Part 1: " + field1.count(_ == true))


    val commands2 = toCommands(input) {
        case "turn on" => (s: Int) => s + 1
        case "turn off" => (s: Int) => max(0, s - 1)
        case "toggle" => (s: Int) => s + 2
    }

    val field2 = applyCommands(Array.ofDim(1000 * 1000), 1000, commands2)

    println("Part 2: " + field2.sum)

}