package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

object Day05 extends AoCApp {

    case class Seat(row: Int, column: Int)

    def partitionBit(c: Char) = c match {
        case 'F' | 'L' => 0
        case 'B' | 'R' => 1
    }

    val parseFrontBack = parseOneOf("FB")
        .map(partitionBit)
        .repeated(7)

    val parseLeftRight = parseOneOf("LR")
        .map(partitionBit)
        .repeated(3)

    def foldToInt(nums: Seq[Int]) = nums.foldLeft(0)((agg, i) => (agg << 1) | i)

    val parseSeatPath = for {
        frontBack <- parseFrontBack
        leftRight <- parseLeftRight
    } yield Seat(foldToInt(frontBack), foldToInt(leftRight))

    val parseInput = for {
        first <- parseSeatPath
        following <- parseAll(parseWhitespace.?.flatMap(_ => parseSeatPath))
    } yield first +: following

    val sortedSeatIds = parse(Input2020.Day05, parseInput).map(s => s.row * 8 + s.column).sorted

    val result = sortedSeatIds.last
    part(1, result)

    val mySeatId = sortedSeatIds.sliding(2).flatMap {
        case Seq(a, b) => if (a + 1 == b) None else Some(a + 1)
    }.next()

    part(2, mySeatId)
}
