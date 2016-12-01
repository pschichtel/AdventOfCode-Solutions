package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp


object Day03 extends AoCApp {
    println("Day  3")

    val input = inputSource.mkString.trim
    val directions = input map {
        case '^' => (0, 1)
        case '>' => (1, 0)
        case 'v' => (0, -1)
        case '<' => (-1, 0)
    }

    def add(a: (Int, Int), b: (Int, Int)) = (a._1 + b._1, a._2 + b._2)

    val (_, lastYearsSet) = directions.foldLeft(((0, 0), Set.empty[(Int, Int)])) {
        case ((a, set), b) => (add(a, b), set + a)
    }

    println("Part 1: " + lastYearsSet.size)


    val (_, _, thisYearsSet) = directions.zipWithIndex.foldLeft((0, 0), (0, 0), Set.empty[(Int, Int)]) {
        case ((a, b, set), (c, i)) => if (i % 2 == 0) (add(a, c), b, set + a) else (a, add(b, c), set + b)
    }

    println("Part 2: " + thisYearsSet.size)
}
