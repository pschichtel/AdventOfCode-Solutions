package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

object Day05 extends AoCApp {

    case class Seat(row: Int, column: Int)

    def partitionBit(c: Char): Int = c match {
        case 'F' | 'L' => 0
        case 'B' | 'R' => 1
    }

    private val parseFrontBack = parseOneOf("FB")
        .map(partitionBit)
        .repeated(7)

    private val parseLeftRight = parseOneOf("LR")
        .map(partitionBit)
        .repeated(3)

    def foldToInt(nums: Seq[Int]): Int = nums.foldLeft(0)((agg, i) => (agg << 1) | i)

    private val parseSeatPath = for {
        frontBack <- parseFrontBack
        leftRight <- parseLeftRight
    } yield Seat(foldToInt(frontBack), foldToInt(leftRight))

    private val parseInput = for {
        first <- parseSeatPath
        following <- parseAll(parseWhitespace.?.flatMap(_ => parseSeatPath))
    } yield first +: following

    override def solution: (Any, Any) = {
        val sortedSeatIds = parse(Input2020.Day05, parseInput).map(s => s.row * 8 + s.column).sorted

        val result = sortedSeatIds.last

        val mySeatId = sortedSeatIds.sliding(2).flatMap {
            case Seq(a, b) => if (a + 1 == b) None else Some(a + 1)
        }.next()

        (result, mySeatId)
    }
}
