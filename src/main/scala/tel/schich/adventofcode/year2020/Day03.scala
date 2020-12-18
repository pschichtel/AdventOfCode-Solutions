package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day03 extends AoCApp {

    def countTrees(grid: Seq[Seq[Boolean]], width: Int, dx: Int, dy: Int): Int = {
        @tailrec
        def loop(x: Int, y: Int, count: Int): Int = {
            if (y >= grid.size) count
            else if (grid(y)(x % width)) loop(x + dx, y + dy, count + 1)
            else loop(x + dx, y + dy, count)
        }

        loop(dx, dy, 0)
    }

    override def solution: (Any, Any) = {

        val grid = asLines(Input2020.Day03).map(_.map {
            case '#' => true
            case '.' => false
        })

        val gridColumnWidth = grid.head.size

        val part1 = countTrees(grid, gridColumnWidth, 3, 1)

        val slopes = Seq(
            (1, 1),
            (3, 1),
            (5, 1),
            (7, 1),
            (1, 2),
        )

        val allSlopeProduct = slopes.foldLeft(1L) {
            case (agg, (dx, dy)) => agg * countTrees(grid, gridColumnWidth, dx, dy)
        }

        val part2 = allSlopeProduct

        (part1, part2)
    }
}
