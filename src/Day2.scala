import scala.io.Source
import scala.math._

object Day2 extends AoCApp {

    println("Day  2")

    val source = sourceFromCP("day2.txt").mkString
    val values = source.trim.split('\n').map(_.trim).map(_.split('x')).map(x => (x(0).toInt, x(1).toInt, x(2).toInt))

    val area = values.map { case (x, y, z) => 2 * x * y + 2 * y * z + 2 * x * z + min(x * y, min(y * z, x * z)) }.sum

    println("Part 1: " + area)


    def mid(x: Int, y: Int, z: Int) = {
        if (x == y && y == z || x == y || x == z) x
        else if (y == z) y
        else (x + y + z) - min(x, min(y, z)) - max(x, max(y, z))
    }

    val ribbon = values.map { case (x, y, z) => 2 * min(x, min(y, z)) + 2 * mid(x, y, z) + x * y * z }.sum

    println("Part 2: " + ribbon)
}