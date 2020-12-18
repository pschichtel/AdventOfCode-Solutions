package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day06 extends AoCApp {
    def decode(input: Seq[String], dir: Int = -1): String = {
        (0 until input.head.length).map {
            i => input.map(_(i)).groupBy(identity).toSeq.sortBy(dir * _._2.length).map(_._1).head
        }.mkString
    }

    override def solution: (Any, Any) = {
        val input = asLines(Input2016.Day06).toList

        (decode(input), decode(input, 1))
    }
}
