package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day24 extends AoCApp {

    def findMinimumQuantumEntanglement(weights: Seq[Int], compartments: Int): Long = {
        val maxWeightPerCompartment = weights.sum / compartments
        val maxElementsPerCompartment = math.ceil(weights.length / compartments).toInt

        def validCombinationsOfBags(size: Int): Iterator[(Int, Long)] = {
            weights.combinations(size)
                .filter(_.sum == maxWeightPerCompartment)
                .map(combination => (combination.length, combination.map(_.toLong).product))
        }

        (2 to maxElementsPerCompartment)
            .flatMap(validCombinationsOfBags)
            .min
            ._2
    }

    override def solution: (Any, Any) = {
        val packageWeights = asLines(Input2015.Day24).map(_.toInt)

        //    val inputsExample = Seq(1, 2, 3, 4, 5, 7, 8, 9, 10, 11)
        //    val inputsExampleWin = Seq(10, 9, 1, 11, 7, 2, 8, 5, 4, 3)

        // combinations(x, y) = (2 to ceil(x/y)).map(i => y choose i).sum
        val part1 = findMinimumQuantumEntanglement(packageWeights, 3) // combinations(29, 3) ~ 36.5M
        val part2 = findMinimumQuantumEntanglement(packageWeights, 4) // combinations(29, 4) ~  6.5M

        (part1, part2)
    }
}
