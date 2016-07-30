import scala.annotation.tailrec
import scala.util.matching.Regex

object Day11 extends AoCApp {
    println("Day 11")

    val input = "cqjxjnds"

    @tailrec
    private def increment(s: String, suffix: String = ""): String = {

        s.last match {
            case 'z' => increment(s.dropRight(1), 'a' + suffix)
            case c => s.dropRight(1) + (c + 1).toChar + suffix
        }

    }

    def streamIncrements(s: String): Stream[String] = s #:: streamIncrements(increment(s))

    def hasIncreasing(s: String, n: Int = 3) = {
        s.sliding(n).exists(_.sliding(2).forall { w =>
            w(0) + 1 == w(1).toInt
        })
    }

    def containsForbidden(s: String, forbidden: Set[Char] = Set('i', 'o', 'l')) = {
        s.exists(forbidden.contains)
    }

    val LetterPair = raw"(.)\1".r

    def containsLetterPairs(s: String, r: Regex = LetterPair, n: Int = 2) = {
        r.findAllIn(s).size >= n
    }

    def stepOneCond(s: String) = {
        hasIncreasing(s) && !containsForbidden(s) && containsLetterPairs(s)
    }

    val stream = streamIncrements(input).filter(stepOneCond)

    val firstPass = stream.head
    println(s"Part 1: $firstPass")

    val secondPass = stream.drop(1).head
    println(s"Part 2: $secondPass")

}