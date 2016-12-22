package tel.schich.adventofcode.year2015

import java.util.concurrent.atomic.AtomicInteger

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day20.N

object Day25 extends AoCApp {

    def generateDiagonal(n: Int): Stream[(Int, Int)] = N.take(n).map(i => (i, n - (i - 1)))
    def generateGrid: Stream[(Int, Int)] = N.flatMap(generateDiagonal)

    val List(y, x) = "\\d+".r.findAllIn(inputText).toList
    val targetPos = (x.toInt, y.toInt)

    val startCode = 20151125L
    def nextCode(currentCode: Long): Long = (currentCode * 252533) % 33554393

    val i = new AtomicInteger(1)
    println(generateGrid.takeWhile(pos => pos != targetPos).foldLeft(startCode) { (current, pos) =>
        val next = nextCode(current)
        println(s"${i.getAndIncrement()} -> $pos: $current -> $next")
        next
    })

}
