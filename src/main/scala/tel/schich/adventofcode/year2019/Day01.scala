package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day01 extends AoCApp {

    def calculateFuel(mass: Int): Int = mass / 3 - 2

    @tailrec
    def calculateFuelFuel(mass: Int, accumulatedFuel: Int): Int = {
        val additionalRequiredFuel = calculateFuel(mass)
        if (additionalRequiredFuel <= 0) accumulatedFuel
        else calculateFuelFuel(additionalRequiredFuel, accumulatedFuel + additionalRequiredFuel)
    }
    override def solution: (Any, Any) = {
        val masses = asLines(Input2019.Day01).map(_.toInt)

        val part1 = masses.map(calculateFuel).sum
        val part2 = masses.map(calculateFuelFuel(_: Int, 0)).sum

        (part1, part2)
    }
}
