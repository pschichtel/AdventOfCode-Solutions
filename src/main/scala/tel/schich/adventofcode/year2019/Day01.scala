package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec

object Day01 extends AoCApp {

    def calculateFuel(mass: Int) = mass / 3 - 2

    val masses = inputLines.map(_.toInt)

    part(1, masses.map(calculateFuel).sum)

    @tailrec
    def calculateFuelFuel(mass: Int, accumulatedFuel: Int): Int = {
        val additionalRequiredFuel = calculateFuel(mass)
        if (additionalRequiredFuel <= 0) accumulatedFuel
        else calculateFuelFuel(additionalRequiredFuel, accumulatedFuel + additionalRequiredFuel)
    }

    part(2, masses.map(calculateFuelFuel(_: Int, 0)).sum)
}
