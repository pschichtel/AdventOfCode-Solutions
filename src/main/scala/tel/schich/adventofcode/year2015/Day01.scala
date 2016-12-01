package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp


object Day01 extends AoCApp {
    println("Day  1")

    val input = inputSource.mkString.trim

    val rel = input map {
        case '(' => 1
        case ')' => -1
    }

    println("Part 1: " + rel.sum)

    println("Part 2: " + (rel.zipWithIndex.foldLeft((0, 0)) {
        case ((-1, out: Int), _) => (-1, out)
        case ((floor: Int, _), (direction: Int, in: Int)) =>
            (floor + direction, in)
    }._2 + 1))
}
