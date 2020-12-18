package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

import java.lang.Integer.bitCount

object Day06 extends AoCApp {

    private val parsePerson = parseWord.map { person =>
        person.view.foldLeft(0)((bitSet, c) => bitSet | (1 << ('a' - c)))
    }
    private val parseGroup = parseAllSeparated(parsePerson, parseLineBreak)
    private val parseInput = parseAllSeparated(parseGroup, parseLineBreak.repeated(2))

    def solve(groups: Seq[Seq[Int]], f: (Int, Int) => Int): Int =
        groups.foldLeft(0)((sum, group) => sum + bitCount(group.reduce(f)))

    override def solution: (Any, Any) = {
        val groups = parse(Input2020.Day06, parseInput)

        (solve(groups, _ | _), solve(groups, _ & _))
    }
}
