package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.generated.Input2020
import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser
import tel.schich.adventofcode.shared.Parser._

object Day02 extends AoCApp {

    val parseLine = for {
        n1 <- parseNaturalNumber.map(_.toInt)
        _ <- parseString("-")
        n2 <- parseNaturalNumber.map(_.toInt)
        _ <- parseSpaces
        letter <- parseChar
        _ <- parseString(": ")
        password <- parseWhile(_.isLetterOrDigit)
    } yield (n1, n2, letter, password)

    val parseInput: Parser[Seq[(Int, Int, Char, String)]] = parseAllSeparated(parseLine, parseLineBreak)

    private val entries = parse(Input2020.Day02, parseInput)

    part(1, entries.count {
        case (min, max, letter, password) =>
            val letterCount = password.count(_ == letter)
            letterCount >= min && letterCount <= max
    })

    part(2, entries.count {
        case (first, second, letter, password) =>
            (password(first - 1) == letter) ^ (password(second - 1) == letter)
    })
}
