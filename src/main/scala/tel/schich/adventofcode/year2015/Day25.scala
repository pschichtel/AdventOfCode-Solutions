package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec

object Day25 extends AoCApp {

    @inline
    def nextCode(currentCode: Long): Long = (currentCode * 252533) % 33554393

    def findCode(startCode: Long, column: Int, row: Int): Long = {

        @tailrec
        def findNext(currentCode: Long, diagonal: Int, x: Int): Long = {
            if (column == x && row == diagonal - (x - 1)) currentCode
            else {
                val next = nextCode(currentCode)
                if (x == diagonal) findNext(next, diagonal + 1, 1)
                else findNext(next, diagonal, x + 1)
            }
        }

        findNext(startCode, 1, 1)
    }

    val List(y, x) = "\\d+".r.findAllIn(inputText).toList
    val targetPos = (x.toInt, y.toInt)

    part(1, findCode(20151125L, x.toInt, y.toInt))
}
