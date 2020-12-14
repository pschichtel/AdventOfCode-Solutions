package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

object Day13 extends AoCApp {

    val parseEstimate = parseNaturalNumber.map(_.toInt)
    val parseBusLine = parseString("x").or(parseNaturalNumber.map(_.toInt)).map(_.toOption)
    val parseBusLines = parseAllSeparated(parseBusLine, parseString(","))
    val parseInput = for {
        estimate <- parseEstimate
        _ <- parseLineBreak
        busLines <- parseBusLines
        _ <- parseAll(parseLineBreak)
    } yield (estimate, busLines)

    val (estimate, busLines) = parse(Input2020.Day13, parseInput)

    val (line, delay) = busLines.flatten.map { line =>
        (line, ((math.ceil(estimate / line) * line).toInt + line) - estimate)
    }.minBy(_._2)

    part(1, line * delay)

    val linesWithOffset = busLines.zipWithIndex
        .collect { case (Some(line), offset) => (line, offset) }

    def multiplesOf[T](n: T, from: T)(implicit int: Numeric[T]): LazyList[T] = from #:: multiplesOf(n, int.plus(from, n))(int)
}
