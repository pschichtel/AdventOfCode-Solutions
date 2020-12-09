package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.math.Ordering.Double.IeeeOrdering
import scala.math.Pi

object Day10 extends AoCApp {

    type Point = (Int, Int)

    case class UnorderedPair[A, B](_1: A, _2: B) {
        override def equals(obj: Any): Boolean = {
            obj match {
                case UnorderedPair(oa, ob) => _1 == oa && _2 == ob || _1 == ob && _2 == oa
                case _ => false
            }
        }

        override def hashCode(): Int = _1.hashCode() + _2.hashCode()
    }

    val asteroids = for {
        (line, y) <- asLines(Input2019.Day10).zipWithIndex
        x <- line.indices
        if line(x) == '#'
    } yield (x, y)

    val (monitor, visibleAsteroids) = asteroids.map(a => (a, uniqueAngles(a, asteroids).size)).maxBy(_._2)
    part(1, visibleAsteroids)

    val sortedByAngleAndDistance = angles(monitor, asteroids)
        .groupBy(_._2)
        .toSeq
        .flatMap(a => a._2.sortBy(t => distance(monitor, t._1))(IeeeOrdering).zipWithIndex.map(t => (t._1._1, a._1 + t._2 * 360.0)))
        .sortBy(t => t._2)(IeeeOrdering)
    part(2, sortedByAngleAndDistance.drop(199).headOption.map(_._1).map(t => t._1 * 100 + t._2).getOrElse(0))

    def distance(a: Point, b: Point): Double = a._1 * b._1 + a._2 * b._2

    def uniqueAngles(center: Point, all: Seq[Point]): Set[Double] =
        angles(center, all).map(_._2).toSet

    // moves the center point to (0, 0) and calculates the angle
    def angles(center: Point, all: Seq[Point]): Seq[(Point, Double)] = {
        for {
            p <- all
            if p != center
        } yield (p, angleBetween(center, p))
    }

    def angleBetween(a: Point, b: Point): Double = {
        angle(b._1 - a._1, b._2 - a._2)
    }

    def angle(x: Int, y: Int): Double = {
        val Tau = 2 * Pi
        // calculate angle flipped on the x axis, as we are below the it
        val half = -math.atan2(y, x)
        val full =
            if (half < 0) half + Tau
            else half
        // invert rotation, rotate by 90° to have 0° facing up
        math.toDegrees(((Tau - full) + Pi / 2.0) % Tau)
    }

}
