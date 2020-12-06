package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser
import tel.schich.adventofcode.shared.Parser._

object Day06 extends AoCApp {

    val parsePerson = parseAtLeastOnce(_.isLetter).map(_.toSet)
    val parseGroup = parseAllSeparated(parsePerson, parseLineBreak)

    implicit val parseInput: Parser[Seq[Seq[Set[Char]]]] =
        parseAllSeparated(parseGroup, parseLineBreak.repeated(2))

    val groups = input(parseInput)

    def solve[A](groups: Seq[Seq[Set[Char]]], f: (Set[Char], Set[Char]) => Set[Char]): Int = groups.map { group =>
        group.reduce(f).size
    }.sum

    part(1, solve(groups, _ ++ _))
    part(2, solve(groups, _ intersect _))

}
