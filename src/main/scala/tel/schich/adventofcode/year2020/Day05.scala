package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser
import tel.schich.adventofcode.shared.Parser._

object Day05 extends AoCApp {

    case class Seat(row: Int, column: Int)

    val parseFrontBack = parseOneOf("FB").map {
        case 'F' => 0
        case 'B' => 1
    }.repeated(7)

    val parseLeftRight = parseOneOf("LR").map {
        case 'L' => 0
        case 'R' => 1
    }.repeated(3)

    def foldToInt(nums: Seq[Int]) = nums.foldLeft(0)((agg, i) => (agg << 1) | i)

    val parseSeatPath = for {
        frontBack <- parseFrontBack
        leftRight <- parseLeftRight
    } yield Seat(foldToInt(frontBack), foldToInt(leftRight))

    implicit val parseInput: Parser[Seq[Seat]] = for {
        first <- parseSeatPath
        following <- parseAll(parseWhitespace.?.flatMap(_ => parseSeatPath))
    } yield first +: following

    val sortedSeatIds = input[Seq[Seat]].map(s => s.row * 8 + s.column).sorted

    val result = sortedSeatIds.last
    part(1, result)

    val mySeatId = sortedSeatIds.sliding(2).flatMap {
        case Seq(a, b) => if (a + 1 == b) None else Some(a + 1)
    }.next()

    part(2, mySeatId)
}
