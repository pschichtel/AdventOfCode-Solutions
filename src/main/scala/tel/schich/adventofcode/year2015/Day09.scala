package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day09 extends AoCApp {

    type Path = Seq[String]

    def buildPaths(edgesByOrigin: Map[String, Set[(String, String, Int)]], node: String, path: Path = Nil): Set[Path] = {
        val toVisit = edgesByOrigin.getOrElse(node, Set.empty).map(_._2) -- path - node

        val next = path :+ node
        if (toVisit.isEmpty) Set(next)
        else toVisit.flatMap(visit => buildPaths(edgesByOrigin, visit, next))
    }

    def pathLength(distanceBetween: Map[(String, String), Int], p: Path): Int = p.sliding(2).map(w => distanceBetween((w.head, w(1)))).sum

    override def solution: (Any, Any) = {

        val input = asLines(Input2015.Day09)

        val edge = "(\\w+) to (\\w+) = (\\d+)".r

        val edges = input.flatMap {
            case edge(from, to, distance) =>
                val dist = distance.toInt
                Seq((from, to, dist), (to, from, dist))
        }.toSet

        val nodes = edges.foldLeft(Set.empty[String]) {
            case (set, (from, to, _)) => set + from + to
        }

        val edgesByOrigin = edges.groupBy(_._1)
        val distanceBetween = edges.map(e => (e._1, e._2) -> e._3).toMap

        val paths = nodes.flatMap(buildPaths(edgesByOrigin, _)).filter(p => p.size == nodes.size)
        val pathsWithLength = paths.map(p => (p, pathLength(distanceBetween, p)))

        val (_, minLength) = pathsWithLength.minBy(_._2)
        val (_, maxLength) = pathsWithLength.maxBy(_._2)

        (minLength, maxLength)
    }
}
