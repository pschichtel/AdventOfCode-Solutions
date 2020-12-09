package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.generated.Input2016
import tel.schich.adventofcode.shared.AoCApp

object Day02 extends AoCApp {

    type Pos = (Int, Int)

    def toVector(instr: Char): Pos = {
        instr match {
            case 'U' => ( 0,  1)
            case 'D' => ( 0, -1)
            case 'L' => (-1,  0)
            case 'R' => ( 1,  0)
        }
    }

    def add(left: Pos, right: Pos): Pos = (left._1 + right._1, left._2 + right._2)

    def decode(keyPadSpec: PartialFunction[Pos, Char], instructions: Seq[String], start: Pos): String = {

        def safeAdd(left: Pos, right: Pos): Pos = {
            val newPos = add(left, right)
            if (keyPadSpec.isDefinedAt(newPos)) newPos
            else left
        }

        val (_, finalCode) = instructions.foldLeft((start, "")) {
            case ((pos, code), instructionsForNumber) =>
                val newPos = instructionsForNumber.map(toVector).foldLeft(pos)(safeAdd)
                (newPos, code + keyPadSpec(newPos))
        }

        finalCode
    }

    val ImaginedKeypad = Map(
        (0, 2) -> '1', (1, 2) -> '2', (2, 2) -> '3',
        (0, 1) -> '4', (1, 1) -> '5', (2, 1) -> '6',
        (0, 0) -> '7', (1, 0) -> '8', (2, 0) -> '9'
    )

    val instructions = asLines(Input2016.Day02)

    part(1, decode(ImaginedKeypad, instructions, (1, 1)))

    val RealKeypad = Map(
                                      (2, 4) -> '1',
                       (1, 3) -> '2', (2, 3) -> '3', (3, 3) -> '4',
        (0, 2) -> '5', (1, 2) -> '6', (2, 2) -> '7', (3, 2) -> '8', (4, 2) -> '9',
                       (1, 1) -> 'A', (2, 1) -> 'B', (3, 1) -> 'C',
                                      (2, 0) -> 'D'
    )

    part(2, decode(RealKeypad, instructions, (0, 2)))
}
