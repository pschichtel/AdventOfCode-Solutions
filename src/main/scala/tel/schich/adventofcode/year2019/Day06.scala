package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.collection.immutable.ArraySeq

object Day06 extends AoCApp {

    def countTransitiveConnections(descendentsMap: Map[String, ArraySeq[String]], node: String, depth: Int): Int = {
        val descendents = descendentsMap.getOrElse(node, Seq.empty)
        if (descendents.isEmpty) depth
        else depth + descendents.map(d => countTransitiveConnections(descendentsMap, d, depth + 1)).sum
    }

    def calculateDistance(descendentsMap: Map[String, ArraySeq[String]], node: String, between: (String, String), depth: Int): Int = {
        val descendents = descendentsMap.getOrElse(node, Seq.empty)
        if (descendents.isEmpty) {
            if (node == between._1 || node == between._2) depth
            else -1
        } else {
            descendents.map(d => calculateDistance(descendentsMap, d, between, depth + 1)).filter(_ != -1) match {
                case s if s.length == 2 => s.map(_ - 1 - depth).sum
                case Seq(a) => a
                case _ => -1
            }
        }
    }

    override def solution: (Any, Any) = {
        val descendentsMap = asLines(Input2019.Day06)
            .map(_.split(')'))
            .groupBy(_(0))
            .view
            .mapValues(_.map(_(1)))
            .toMap

        val part1 = countTransitiveConnections(descendentsMap, "COM", 0)
        val part2 = calculateDistance(descendentsMap, "COM", ("YOU", "SAN"), 0)

        (part1, part2)
    }
}
