package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.math._

object Day20 extends AoCApp {

    def numberOfPresentsFor(house: Int, factor: Int, f: (Int, Int) => Boolean): Int = {
        val divisors = (1 to sqrt(house).toInt).filter(house % _ == 0).map {d =>
            val base = if (f(house, d)) d else 0
            val second = house / d
            base + (if (f(house, second)) second else 0)
        }
        factor * divisors.sum
    }

    override def solution: (Any, Any) = {
        val input = Input2015.Day20.toInt

        val (house, _) = (1 to input / 10).map(house => (house, numberOfPresentsFor(house, 10, (_, _) => true)))
            .filter {case (_, presents) => presents >= input}
            .head

        val (houseLimited, _) = (1 to input / 10).map(house => (house, numberOfPresentsFor(house, 11, _ / _ < 50)))
            .filter {case (_, presents) => presents >= input}
            .head

        (house, houseLimited)
    }
}