package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day01 extends AoCApp {

    val input = Input2015.Day01.trim

    val rel = input map {
        case '(' => 1
        case ')' => -1
    }

    part(1, rel.sum)

    part(2, rel.zipWithIndex.foldLeft((0, 0)) {
        case ((-1, out: Int), _) => (-1, out)
        case ((floor: Int, _), (direction: Int, in: Int)) =>
            (floor + direction, in)
    }._2 + 1)
}
