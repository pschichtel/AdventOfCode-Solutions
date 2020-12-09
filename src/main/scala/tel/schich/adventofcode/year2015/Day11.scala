package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.generated.Input2015
import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day11 extends AoCApp {

    @tailrec
    private def increment(s: String, suffix: String = ""): String = {

        s.last match {
            case 'z' => increment(s.dropRight(1), s"a$suffix")
            case c => s.dropRight(1) + (c + 1).toChar + suffix
        }

    }

    def streamIncrements(s: String): LazyList[String] = s #:: streamIncrements(increment(s))

    def hasIncreasing(s: String, n: Int = 3): Boolean = {
        s.toSeq.sliding(n).exists(_.sliding(2).forall { w =>
            w(0) + 1 == w(1).toInt
        })
    }

    def containsForbidden(s: String, forbidden: Set[Char] = Set('i', 'o', 'l')): Boolean = {
        s.exists(forbidden.contains)
    }

    val LetterPair = raw"(.)\1".r

    def containsLetterPairs(s: String, r: Regex = LetterPair, n: Int = 2): Boolean = {
        r.findAllIn(s).size >= n
    }

    def stepOneCond(s: String): Boolean = {
        hasIncreasing(s) && !containsForbidden(s) && containsLetterPairs(s)
    }

    val stream = streamIncrements(Input2015.Day11).filter(stepOneCond)

    val firstPass = stream.head
    part(1, firstPass)

    val secondPass = stream.drop(1).head
    part(2, secondPass)

}