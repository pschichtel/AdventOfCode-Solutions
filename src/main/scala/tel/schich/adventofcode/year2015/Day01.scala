package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day01 extends AoCApp {
    override def solution: (Any, Any) = {
        val input = Input2015.Day01

        val rel = input map {
            case '(' => 1
            case ')' => -1
        }

        val part1 = rel.sum

        val part2 = rel.zipWithIndex.foldLeft((0, 0)) {
            case ((-1, out: Int), _) => (-1, out)
            case ((floor: Int, _), (direction: Int, in: Int)) =>
                (floor + direction, in)
        }._2 + 1

        (part1, part2)
    }
}
