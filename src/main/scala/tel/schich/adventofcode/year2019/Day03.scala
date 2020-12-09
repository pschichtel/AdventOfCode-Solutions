package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day03 extends AoCApp {
    type Point = (Int, Int)

    val Seq(line1, line2) = asLines(Input2019.Day03)
        .map(_.split(',').toList)
        .map(rel => translateToPoints(rel, 0, 0, 0, 0, 0, Vector.empty))

    val intersectionPoints = line1.intersect(line2).toSet

    part(1, intersectionPoints.map(p => math.abs(p._1) + math.abs(p._2)).min)

    val line1StepsToIntersections = countStepsForLine(line1.toList, intersectionPoints, 1, Map.empty)
    val line2StepsToIntersections = countStepsForLine(line2.toList, intersectionPoints, 1, Map.empty)

    part(2, intersectionPoints.map(p => line1StepsToIntersections(p) + line2StepsToIntersections(p)).min)

    @tailrec
    def translateToPoints(relative: List[String], cx: Int, cy: Int, x: Int, y: Int, direction: Int, points: Vector[Point]): Vector[Point] = {
        if (x > 0) translateToPoints(relative, cx + direction, cy, x - 1, y, direction, points :+ ((cx + direction, cy)))
        else if (y > 0) translateToPoints(relative, cx, cy + direction, x, y - 1, direction, points :+ ((cx, cy + direction)))
        else relative match {
            case head :: tail =>
                val n = head.substring(1).toInt
                head(0) match {
                    case 'U' => translateToPoints(tail, cx, cy, 0, n, 1, points)
                    case 'D' => translateToPoints(tail, cx, cy, 0, n, -1, points)
                    case 'R' => translateToPoints(tail, cx, cy, n, 0, 1, points)
                    case 'L' => translateToPoints(tail, cx, cy, n, 0, -1, points)
                }
            case Nil =>
                points
        }
    }

    @tailrec
    def countStepsForLine(points: List[Point], intersections: Set[Point], steps: Int, stepMap: Map[Point, Int]): Map[Point, Int] = {
        if (intersections.isEmpty) stepMap
        else points match {
            case head :: tail =>
                if (intersections.contains(head)) countStepsForLine(tail, intersections - head, steps + 1, stepMap + (head -> steps))
                else countStepsForLine(tail, intersections, steps + 1, stepMap)
            case Nil => stepMap
        }
    }
}
