package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.AoCApp
import Day02.{InstructionSet, comparisonOp, instructions, jumpIf, parseProgram, runProgram}

object Day05 extends AoCApp {

    val program = parseProgram(inputText)

    part(1, runProgram(instructions, program, 1 :: Nil)._2.last)

    val extendedInstructions: InstructionSet = Map(
        5 -> jumpIf(_ != 0),
        6 -> jumpIf(_ == 0),
        7 -> comparisonOp(_ < _),
        8 -> comparisonOp(_ == _)
    )

    part(2, runProgram(instructions ++ extendedInstructions, program, 5 :: Nil)._2.last)
}
