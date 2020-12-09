package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

object Day06 extends AoCApp {

    val descendentsMap = asLines(Input2019.Day06)
        .map(_.split(')'))
        .groupBy(_(0))
        .view
        .mapValues(_.map(_(1)))
        .toMap

    part(1, countTransitiveConnections("COM", 0))
    part(2, calculateDistance("COM", ("YOU", "SAN"), 0))

    def countTransitiveConnections(node: String, depth: Int): Int = {
        val descendents = descendentsMap.getOrElse(node, Seq.empty)
        if (descendents.isEmpty) depth
        else depth + descendents.map(d => countTransitiveConnections(d, depth + 1)).sum
    }

    def calculateDistance(node: String, between: (String, String), depth: Int): Int = {
        val descendents = descendentsMap.getOrElse(node, Seq.empty)
        if (descendents.isEmpty) {
            if (node == between._1 || node == between._2) depth
            else -1
        } else {
            descendents.map(d => calculateDistance(d, between, depth + 1)).filter(_ != -1) match {
                case s if s.length == 2 => s.map(_ - 1 - depth).sum
                case Seq(a) => a
                case _ => -1
            }
        }
    }
}
