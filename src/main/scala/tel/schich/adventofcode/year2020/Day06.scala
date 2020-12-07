package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

import java.lang.Integer.bitCount

object Day06 extends AoCApp {

    val parsePerson = parseAtLeastOnce(_.isLetter).map { person =>
        person.toSeq.map(c => 1 << ('a' - c)).foldLeft(0)(_ | _)
    }
    val parseGroup = parseAllSeparated(parsePerson, parseLineBreak)
    val parseInput = parseAllSeparated(parseGroup, parseLineBreak.repeated(2))

    val groups = input(parseInput)

    def solve(groups: Seq[Seq[Int]], f: (Int, Int) => Int) =
        groups.map(group => bitCount(group.reduce(f))).sum

    part(1, solve(groups, _ | _))
    part(2, solve(groups, _ & _))

}
