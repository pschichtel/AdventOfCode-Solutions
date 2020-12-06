package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

object Day01 extends AoCApp {

    val values = inputLines.map(_.toInt)

    val part1Vales = for {
        a <- values
        b <- values
        if a + b == 2020
    } yield a * b

    part(1, part1Vales.head)

    val part2Vales = for {
        a <- values
        b <- values
        c <- values
        if a + b + c == 2020
    } yield a * b * c

    part(2, part2Vales.head)
}
