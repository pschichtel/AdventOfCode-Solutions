package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day04 extends AoCApp {

    type Num = IndexedSeq[Int]

    @tailrec
    def spans(s: String, acc: Seq[String]): Seq[String] = {
        if (s.isEmpty) acc
        else {
            val (prefix, suffix) = s.span(_ == s.head)
            spans(suffix, acc :+ prefix)
        }
    }

    override def solution: (Any, Any) = {
        val Array(lower, upper) = Input2019.Day04.split('-').map(_.toInt)

        val validPasswords = (lower to upper).filter { n =>
            val s = n.toString
            val pairs = s.toSeq.sliding(2).toVector

            val hasDouble = pairs.exists(p => p(0) == p(1))
            val hasNoDecrease = pairs.forall(p => p(1) >= p(0))

            hasDouble && hasNoDecrease
        }

        val part1 = validPasswords.length

        val validPart2Passwords = (lower to upper).filter { n =>
            val s = n.toString

            val hasDouble = spans(s, Vector.empty).exists(span => span.length == 2)
            val hasNoDecrease = s.toSeq.sliding(2).forall(p => p(1) >= p(0))

            hasDouble && hasNoDecrease
        }

        val part2 = validPart2Passwords.length

        (part1, part2)
    }
}
