package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day03 extends AoCApp {

    val triangleSpec = "\\d+"

    val numberLines = inputLines.map(_.split("\\s+").map(_.trim.toInt).toList).toVector

    def validateTriangle(numbers: Seq[Int]): Boolean = {
        numbers.permutations.forall({
            case Seq(a, b, c) => a + b > c
        })
    }

    def countCorrectTriangles(specs: Seq[Seq[Int]]): Int = specs.count(validateTriangle)

    part(1, countCorrectTriangles(numberLines))

    val columnWise = (0 until 3).flatMap(i => numberLines.map(_(i)))
    part(2, countCorrectTriangles(columnWise.grouped(3).toSeq))
}
