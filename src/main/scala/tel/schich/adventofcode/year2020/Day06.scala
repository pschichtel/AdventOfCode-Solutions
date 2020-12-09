package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

import java.lang.Integer.bitCount

object Day06 extends AoCApp {

    val parsePerson = parseWord.map { person =>
        person.view.foldLeft(0)((bitSet, c) => bitSet | (1 << ('a' - c)))
    }
    val parseGroup = parseAllSeparated(parsePerson, parseLineBreak)
    val parseInput = parseAllSeparated(parseGroup, parseLineBreak.repeated(2))

    val groups = parse(Input2020.Day06, parseInput)

    def solve(groups: Seq[Seq[Int]], f: (Int, Int) => Int) =
        groups.foldLeft(0)((sum, group) => sum + bitCount(group.reduce(f)))

    part(1, solve(groups, _ | _))
    part(2, solve(groups, _ & _))

}
