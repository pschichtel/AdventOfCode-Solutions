package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

object Day02 extends AoCApp {

    private val parseLine = for {
        n1 <- parseNaturalNumber.map(_.toInt)
        _ <- parseString("-")
        n2 <- parseNaturalNumber.map(_.toInt)
        _ <- parseSpaces
        letter <- parseChar
        _ <- parseString(": ")
        password <- parseWhile(_.isLetterOrDigit)
    } yield (n1, n2, letter, password)

    private val parseInput = parseAllSeparated(parseLine, parseLineBreak)

    override def solution: (Any, Any) = {
        val entries = parse(Input2020.Day02, parseInput)

        val part1 = entries.count {
            case (min, max, letter, password) =>
                val letterCount = password.view.count(_ == letter)
                letterCount >= min && letterCount <= max
        }

        val part2 = entries.count {
            case (first, second, letter, password) =>
                (password(first - 1) == letter) ^ (password(second - 1) == letter)
        }

        (part1, part2)
    }
}
