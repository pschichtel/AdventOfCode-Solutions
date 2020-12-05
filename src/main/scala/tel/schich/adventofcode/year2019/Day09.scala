package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.year2019.Day02.{InstructionSet, ProgramState, parseProgram, runProgram}

object Day09 extends AoCApp {

    val program = parseProgram(inputText)

    def relativeBaseOffset(state: ProgramState): ProgramState =
        state.continue(
            relativeBase = state.relativeBase + state.readParam(1),
            pc = state.pc + 2
        )

    lazy val instructions: InstructionSet = Day05.instructions ++ Map(
        9L -> relativeBaseOffset
    )

    part(1, runProgram(instructions, program, 1 :: Nil).output.last)
    part(2, runProgram(instructions, program, 2 :: Nil).output.last)

}
