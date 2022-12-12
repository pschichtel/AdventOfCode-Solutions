package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser.*

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

object Day13 extends AoCApp {

    def chineseRemainder[T](congruences: Seq[(T, T)])(implicit int: Integral[T]): T => T = {
        val M = congruences.foldLeft(int.one) { case (product, (m, _)) => product * m }
        val sum = congruences.foldLeft(int.zero) { case (sum, (m, a)) =>
            if (a == int.zero) sum
            else {
                val Mm = M / m
                val b = modularMultiplicativeInverse(Mm, m)
                sum + a * (b * Mm)
            }
        }
        n => M * n + int.abs(sum) % M
    }

    def extendedGcd[T](a: T, b: T)(implicit int: Integral[T]): ((T, T), T) = {
        @tailrec
        def solve(prevR: T, r: T, prevS: T, s: T, prevT: T, t: T): ((T, T), T) = {
            if (r == int.zero) ((prevS, prevT), prevR)
            else {
                val quotient = prevR / r
                solve(
                    r, prevR - quotient * r,
                    s, prevS - quotient * s,
                    t, prevT - quotient * t,
                )
            }
        }

        solve(a, b, int.one, int.zero, int.zero, int.one)
    }

    def modularMultiplicativeInverse[T](n: T, modulus: T)(implicit int: Integral[T]): T =
        extendedGcd(n, modulus)._1._1

    private val parseInput = {
        val parseEstimate = parseNaturalNumber.map(_.toInt)
        val parseBusLine = parseString("x").or(parseNaturalNumber).map(_.toOption)
        val parseBusLines = parseAllSeparated(parseBusLine, parseString(","))

        for {
            estimate <- parseEstimate
            _ <- parseLineBreak
            busLines <- parseBusLines
            _ <- parseAll(parseLineBreak)
        } yield (estimate, busLines)
    }

    override def solution: (Any, Any) = {
        val (estimate, busLines) = parse(Input2020.Day13, parseInput)

        val (line, delay) = busLines.flatten.map { line =>
            (line, ((estimate / line) * line) - estimate + line)
        }.minBy(_._2)

        val part1 = line * delay
        val linesWithOffset = busLines.zipWithIndex
            .collect { case (Some(line), offset) => (line, (line + (line - offset.toLong)) % line) }

        val solutions = chineseRemainder(linesWithOffset)

        val part2 = solutions(0)

        (part1, part2)
    }
}
