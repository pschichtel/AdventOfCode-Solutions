package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

object Day01 extends AoCApp {
    val values = asLines(Input2020.Day01).map(_.toInt).toList

    val part1Values = for {
        a <- values.iterator
        b <- values.iterator
        if a + b == 2020
    } yield a * b
    part(1, part1Values.next())

    val part2Values = for {
        a <- values.iterator
        b <- values.iterator
        c <- values.iterator
        if a + b + c == 2020
    } yield a * b * c

    part(2, part2Values.next())
}
