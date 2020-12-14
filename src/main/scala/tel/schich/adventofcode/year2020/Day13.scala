package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

import scala.annotation.tailrec

object Day13 extends AoCApp {

    def chineseRemainder[T](congruences: Seq[(T, T)])(implicit int: Integral[T]): T => T = {
        val M = congruences.foldLeft(int.one) { case (product, (m, _)) => int.times(product, m) }
        val sum = congruences.foldLeft(int.zero) { case (sum, (m, a)) =>
            if (a == int.zero) sum
            else {
                val Mm = int.quot(M, m)
                val b = modularMultiplicativeInverse(Mm, m)
                int.plus(sum, int.times(a, int.times(b, Mm)))
            }
        }
        n => int.plus(int.times(M, n), int.rem(int.abs(sum), M))
    }

    def extendedGcd[T](a: T, b: T)(implicit int: Integral[T]): ((T, T), T) = {
        @tailrec
        def solve(prevR: T, r: T, prevS: T, s: T, prevT: T, t: T): ((T, T), T) = {
            if (r == int.zero) ((prevS, prevT), prevR)
            else {
                val quotient = int.quot(prevR, r)
                solve(
                    r, int.minus(prevR, int.times(quotient, r)),
                    s, int.minus(prevS, int.times(quotient, s)),
                    t, int.minus(prevT, int.times(quotient, t)),
                )
            }
        }

        solve(a, b, int.one, int.zero, int.zero, int.one)
    }

    def modularMultiplicativeInverse[T](n: T, modulus: T)(implicit int: Integral[T]): T =
        extendedGcd(n, modulus)._1._1

    val parseEstimate = parseNaturalNumber.map(_.toInt)
    val parseBusLine = parseString("x").or(parseNaturalNumber).map(_.toOption)
    val parseBusLines = parseAllSeparated(parseBusLine, parseString(","))
    val parseInput = for {
        estimate <- parseEstimate
        _ <- parseLineBreak
        busLines <- parseBusLines
        _ <- parseAll(parseLineBreak)
    } yield (estimate, busLines)

    val (estimate, busLines) = parse(Input2020.Day13, parseInput)

    val (line, delay) = busLines.flatten.map { line =>
        (line, ((math.ceil(estimate / line) * line).toLong + line) - estimate)
    }.minBy(_._2)

    part(1, line * delay)

    val linesWithOffset = busLines.zipWithIndex
        .collect { case (Some(line), offset) => (line, (line + (line - offset.toLong)) % line) }

    val solutions = chineseRemainder(linesWithOffset)
    val solution = solutions(0)

    part(2, solution)
}
