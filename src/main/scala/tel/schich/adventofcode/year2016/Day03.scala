package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day03 extends AoCApp {
    def validateTriangle(numbers: Seq[Int]): Boolean = {
        numbers.permutations.forall({
            case Seq(a, b, c) => a + b > c
        })
    }

    def countCorrectTriangles(specs: Seq[Seq[Int]]): Int = specs.count(validateTriangle)

    override def solution: (Any, Any) = {
        val numberLines = asLines(Input2016.Day03).map(_.split("\\s+").map(_.trim.toInt).toList).toVector
        val part1 = countCorrectTriangles(numberLines)

        val columnWise = (0 until 3).flatMap(i => numberLines.map(_(i)))
        val part2 = countCorrectTriangles(columnWise.grouped(3).toSeq)

        (part1, part2)
    }
}
