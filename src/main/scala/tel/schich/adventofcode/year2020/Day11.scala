package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day11 extends AoCApp {

    val Floor = '.'
    val OccupiedSeat = '#'
    val FreeSeat = 'L'

    val lines = asLines(Input2020.Day11)
    val gridWidth = lines.head.length
    val gridHeight = lines.size
    val startGrid = lines.flatMap(_.toCharArray).toArray

    val neighborDirections = Seq(
        (-1, -1), (0, -1), (1, -1),
        (-1, 0),           (1, 0),
        (-1, 1),  (0, 1),  (1, 1),
    )

    type OccupiedSeatCounter = (Array[Char], Int, Int, Int, Int) => Int

    def simulate(grid: Array[Char], width: Int, height: Int, tolerance: Int, countSurroundingSeats: OccupiedSeatCounter): Array[Char] = {
        val out = Array.ofDim[Char](grid.length)

        for {
            x <- 0 until width
            y <- 0 until height
        } {
            val i = y * width + x
            val current = grid(i)
            out(i) =
                if (current == FreeSeat && countSurroundingSeats(grid, width, height, x, y) == 0) OccupiedSeat
                else if (current == '#' && countSurroundingSeats(grid, width, height, x, y) >= tolerance)  FreeSeat
                else current
        }

        out
    }

    @tailrec
    def simulateUntilStable(grid: Array[Char], width: Int, height: Int, tolerance: Int, countSurroundingSeats: OccupiedSeatCounter): Array[Char] = {
        val next = simulate(grid, width, height, tolerance, countSurroundingSeats)
        if (next sameElements grid) grid
        else simulateUntilStable(next, width, height, tolerance, countSurroundingSeats)
    }

    def countOccupiedNeighbors(grid: Array[Char], width: Int, height: Int, seatX: Int, seatY: Int): Int = {
        neighborDirections
            .count { case (dx, dy) =>
                val nx = seatX + dx
                val ny = seatY + dy
                if (nx >= 0 && nx < width && ny >= 0 && ny < height) grid(ny * width + nx) == OccupiedSeat
                else false
            }
    }

    def countVisibleOccupiedSeats(grid: Array[Char], width: Int, height: Int, seatX: Int, seatY: Int): Int = {
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

    def countOccupiedSeats(grid: Array[Char]) = grid.count(_ == OccupiedSeat)

    part(1, countOccupiedSeats(simulateUntilStable(startGrid, gridWidth, gridHeight, 4, countOccupiedNeighbors)))
    part(2, countOccupiedSeats(simulateUntilStable(startGrid, gridWidth, gridHeight, 5, countVisibleOccupiedSeats)))
}
