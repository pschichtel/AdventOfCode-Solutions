package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.math._

object Day05 extends AoCApp {

    def sliding(s: String, n: Int = 2): Seq[(Int, String)] =
        s.zipWithIndex.map { case (c, i) => (i, s.substring(i, min(s.length, i + n))) }.filter { case (_, s) => s.length == n }

    override def solution: (Any, Any) = {

        val input = asLines(Input2015.Day05)

        val vowels = Set('a', 'e', 'i', 'o', 'u')
        val forbiddenWindows = Set("ab", "cd", "pq", "xy")

        val nice = input.filter { s =>
            val vowelCount = s.count(vowels.contains)
            val windows = s.toSeq.sliding(2).map(_.unwrap).toList

            val doubleWindowCount = windows.count(w => w(0) == w(1))
            val forbiddenWindowCount = windows.count(forbiddenWindows.contains)

            vowelCount >= 3 && doubleWindowCount >= 1 && forbiddenWindowCount == 0
        }

        val nicer = input.filter { s =>
            val reoccurringSlides = sliding(s).collect {
                case (i, w) if s.substring(0, i).contains(w) || s.substring(i + 2).contains(w) => Some(w)
            }

            val triple = s.toSeq.sliding(3).filter(w => w(0) == w(2)).toList.size
            reoccurringSlides.nonEmpty && triple >= 1
        }

        (nice.length, nicer.length)
    }
}
