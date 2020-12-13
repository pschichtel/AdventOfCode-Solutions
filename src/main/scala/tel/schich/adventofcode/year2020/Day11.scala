package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day11 extends AoCApp {

    val Floor = '.'
    val OccupiedSeat = '#'
    val FreeSeat = 'L'

    val lines = asLines(Input2020.Day11)
    val gridWidth = lines.head.length
    val gridHeight = lines.size
    val startGrid = ArraySeq.unsafeWrapArray(lines.flatMap(_.toCharArray).toArray)

    val neighborDirections = Seq(
        (-1, -1), (0, -1), (1, -1),
        (-1, 0),           (1, 0),
        (-1, 1),  (0, 1),  (1, 1),
    )

    type OccupiedSeatCounter = (IndexedSeq[Char], Int, Int, Int, Int) => Int

    def simulate(grid: ArraySeq[Char], width: Int, height: Int, tolerance: Int, countSurroundingSeats: OccupiedSeatCounter): ArraySeq[Char] = {
        grid.zipWithIndex.map { case (current, i) =>
            val y = i / width
            val x = i - (y * width)

            current match {
                case FreeSeat if countSurroundingSeats(grid, width, height, x, y) == 0 => OccupiedSeat
                case OccupiedSeat if countSurroundingSeats(grid, width, height, x, y) >= tolerance => FreeSeat
                case cell => cell
            }
        }
    }

    @tailrec
    def simulateUntilStable(grid: ArraySeq[Char], width: Int, height: Int, tolerance: Int, countSurroundingSeats: OccupiedSeatCounter): IndexedSeq[Char] = {
        val next = simulate(grid, width, height, tolerance, countSurroundingSeats)
        if (next == grid) grid
        else simulateUntilStable(next, width, height, tolerance, countSurroundingSeats)
    }

    def countOccupiedNeighbors(grid: IndexedSeq[Char], width: Int, height: Int, seatX: Int, seatY: Int): Int = {
        neighborDirections
            .count { case (dx, dy) =>
                val nx = seatX + dx
                val ny = seatY + dy
                if (nx >= 0 && nx < width && ny >= 0 && ny < height) grid(ny * width + nx) == OccupiedSeat
                else false
            }
    }

    def countVisibleOccupiedSeats(grid: IndexedSeq[Char], width: Int, height: Int, seatX: Int, seatY: Int): Int = {
        @tailrec
        def rayCast(x: Int, y: Int, dx: Int, dy: Int): Char = {
            val nx = x + dx
            val ny = y + dy
            if (nx >= 0 && nx < width && ny >= 0 && ny < height) {
                val char = grid(ny * width + nx)
                if (char == '.') rayCast(nx, ny, dx, dy)
                else char
            }
            else '.'
        }

        neighborDirections.count { case (dx, dy) => rayCast(seatX, seatY, dx, dy) == '#' }
    }

    def countOccupiedSeats(grid: IndexedSeq[Char]) = grid.count(_ == OccupiedSeat)

    part(1, countOccupiedSeats(simulateUntilStable(startGrid, gridWidth, gridHeight, 4, countOccupiedNeighbors)))
    part(2, countOccupiedSeats(simulateUntilStable(startGrid, gridWidth, gridHeight, 5, countVisibleOccupiedSeats)))
}
