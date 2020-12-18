package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.math._

object Day02 extends AoCApp {

    def mid(x: Int, y: Int, z: Int): Int = {
        if (x == y && y == z || x == y || x == z) x
        else if (y == z) y
        else (x + y + z) - min(x, min(y, z)) - max(x, max(y, z))
    }

    override def solution: (Any, Any) = {
        val values = asLines(Input2015.Day02).map(_.split('x')).map(x => (x(0).toInt, x(1).toInt, x(2).toInt))

        val area = values.map { case (x, y, z) => 2 * x * y + 2 * y * z + 2 * x * z + min(x * y, min(y * z, x * z)) }.sum
        val ribbon = values.map { case (x, y, z) => 2 * min(x, min(y, z)) + 2 * mid(x, y, z) + x * y * z }.sum

        (area, ribbon)
    }
}
