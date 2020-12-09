package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.generated.Input2015
import tel.schich.adventofcode.shared.AoCApp

object Day03 extends AoCApp {

    val input = Input2015.Day03.trim
    val directions = input map {
        case '^' => (0, 1)
        case '>' => (1, 0)
        case 'v' => (0, -1)
        case '<' => (-1, 0)
    }

    def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1 + b._1, a._2 + b._2)

    val (_, lastYearsSet) = directions.foldLeft(((0, 0), Set.empty[(Int, Int)])) {
        case ((a, set), b) => (add(a, b), set + a)
    }

    part(1, lastYearsSet.size)


    val (_, _, thisYearsSet) = directions.zipWithIndex.foldLeft((0, 0), (0, 0), Set.empty[(Int, Int)]) {
        case ((a, b, set), (c, i)) => if (i % 2 == 0) (add(a, c), b, set + a) else (a, add(b, c), set + b)
    }

    part(2, thisYearsSet.size)
}
