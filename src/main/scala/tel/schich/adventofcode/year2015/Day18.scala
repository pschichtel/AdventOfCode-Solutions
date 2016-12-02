package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp

import scala.math._

object Day18 extends AoCApp {
    println("Day 18")

    val input = inputSource.mkString.replaceAll("\\s", "")
    val steps = 100

    val states = input.map {
        case '#' => true
        case '.' => false
    }.toArray

    def step1rule(g: Grid, x: Int, y: Int, s: Boolean, n: Int): Boolean = {
        if (s && n == 2 || n == 3) true
        else if (s) false
        else if (!s && n == 3) true
        else if (!s) false
        else throw new Exception("WHUT?")
    }

    def step2rule(g: Grid, x: Int, y: Int, s: Boolean, n: Int): Boolean = {
        val Max = g.dim - 1
        (x, y) match {
            case (0, 0) => true
            case (Max, 0) => true
            case (Max, Max) => true
            case (0, Max) => true
            case _ => step1rule(g, x, y, s, n)
        }
    }

    case class Grid(states: Seq[Boolean], rule: (Grid, Int, Int, Boolean, Int) => Boolean) {
        val dim: Int = sqrt(states.length).toInt

        val neighbours = Seq(
            (-1, -1), (0, -1), (1, -1),
            (-1, 0), (1, 0),
            (-1, 1), (0, 1), (1, 1)
        )

        def at(x: Int, y: Int): Boolean = {
            if (x < 0 || y < 0 || x >= dim || y >= dim) false
            else states(y * dim + x)
        }

        def atNeighbours(x: Int, y: Int): Seq[Boolean] = {
            neighbours.map { case (nx, ny) => at(x + nx, y + ny) }
        }

        def step(): Grid = {

            val next = for {
                y <- 0 until dim
                x <- 0 until dim
            } yield {
                val s = at(x, y)
                val n = atNeighbours(x, y).count(identity)
                rule(this, x, y, s, n)
            }

            Grid(next, rule)
        }

        def lines: Iterator[Seq[Boolean]] = states.grouped(dim)

        val animation: Stream[Grid] = {
            this #:: animation.map(_.step())
        }
    }

    def print(g: Grid): Unit = {
        println(g.lines.map(_.map(if (_) '#' else '.').mkString).mkString("\n") + "\n")
    }

    val afterNSteps1 = Grid(states, step1rule).animation.drop(steps).head
    part(1, afterNSteps1.states.count(identity))

    val afterNSteps2 = Grid(states, step2rule).animation.drop(steps).head
    part(2,  afterNSteps2.states.count(identity))

}
