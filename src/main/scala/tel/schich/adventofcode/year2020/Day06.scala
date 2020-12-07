package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser
import tel.schich.adventofcode.shared.Parser._

object Day06 extends AoCApp {

    val parsePerson = parseAtLeastOnce(_.isLetter).map { person =>
        person.toSeq.map(1 << ('a' - _)).foldLeft(0)(_ | _)
    }
    val parseGroup = parseAllSeparated(parsePerson, parseLineBreak)

    implicit val parseInput: Parser[Seq[Seq[Int]]] =
        parseAllSeparated(parseGroup, parseLineBreak.repeated(2))

    val groups = input(parseInput)

    def solve[A](groups: Seq[Seq[Int]], f: (Int, Int) => Int): Int = groups.map { group =>
        java.lang.Integer.bitCount(group.reduce(f))
    }.sum

    part(1, solve(groups, _ | _))
    part(2, solve(groups, _ & _))

}
