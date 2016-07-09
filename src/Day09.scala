
object Day09 extends AoCApp {
    println("Day  9")

    val input = sourceFromCP("day09.txt").mkString.trim.split('\n').map(_.trim).toSeq

    val edge = raw"(\w+) to (\w+) = (\d+)".r

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

    type Path = Seq[String]

    def buildPaths(node: String, path: Path = Nil): Set[Path] = {
        val toVisit = edgesByOrigin.getOrElse(node, Set.empty).map(_._2) -- path - node

        val next = path :+ node
        if (toVisit.isEmpty) Set(next)
        else toVisit.flatMap((visit) => buildPaths(visit, next))
    }

    def pathLength(p: Path) = p.sliding(2).map(w => distanceBetween((w.head, w(1)))).sum

    val paths = nodes.flatMap(buildPaths(_)).filter(p => p.size == nodes.size)
    val pathsWithLength = paths.map(p => (p, pathLength(p)))

    val (shortestPath, minLength) = pathsWithLength.minBy(_._2)
    println(s"Part 1: $minLength")

    val (longestPath, maxLength) = pathsWithLength.maxBy(_._2)
    println(s"Part 2: $maxLength")

}
