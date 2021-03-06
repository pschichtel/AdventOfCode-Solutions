package tel.schich.adventofcode.year2019

import Day02.{InstructionSet, ProgramState, binaryOp, parseProgram, runProgram}
import tel.schich.adventofcode.shared.AoCApp

object Day05 extends AoCApp {

    def jumpIf(condition: Long => Boolean)(state: ProgramState): ProgramState =
        if (condition(state.readParam(1))) {
            val newPc = state.readParam(2)
            if (newPc > Int.MaxValue) throw new IllegalArgumentException("A program address can never be larger than a valid 32bit integer!")
            else state.continue(pc = newPc.toInt)
        } else state.continue(pc = state.pc + 3)

    def comparisonOp(comp: (Long, Long) => Boolean)(state: ProgramState): ProgramState =
        binaryOp((a, b) => if (comp(a, b)) 1 else 0)(state)

    lazy val instructions: InstructionSet = Day02.instructions ++ Map(
        5L -> jumpIf(_ != 0),
        6L -> jumpIf(_ == 0),
        7L -> comparisonOp(_ < _),
        8L -> comparisonOp(_ == _)
    )


    override def solution: (Any, Any) = {
        val program = parseProgram(Input2019.Day05)

        val part1 = runProgram(Day02.instructions, program, 1 :: Nil).output.last
        val part2 = runProgram(instructions, program, 5 :: Nil).output.last

        (part1, part2)
    }
}
