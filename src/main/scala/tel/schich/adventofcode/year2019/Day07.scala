package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2019.Day02.{Failed, Memory, Output, ProgramState, SuccessfullyCompleted, initProgram, parseProgram, runProgram}
import tel.schich.adventofcode.year2019.Day05.instructions

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day07 extends AoCApp {

    val program = parseProgram(inputText)

    part(1, findBestPermutation(program, 0 to 4))

    part(2, findBestPermutation(program, 5 to 9))

    def findBestPermutation(program: Memory, modes: Range): Long = {
        val allPermutationsExecuted = modes.permutations.map { modePermutation =>
            val states = Queue.from(modePermutation.map(mode => initProgram(instructions, program, mode :: Nil)))
            runAmplifiers(states, 0 :: Nil).output.head
        }
        allPermutationsExecuted.max
    }

    @tailrec
    def runAmplifiers(queue: Queue[ProgramState], output: Output): ProgramState = {
        val (state, newQueue) = queue.dequeue
        val newState = runProgram(state.continue(input = state.input ++ output))
        val newOutput = newState.output
        newState.status match {
            case SuccessfullyCompleted if newQueue.isEmpty => newState
            case SuccessfullyCompleted => runAmplifiers(newQueue, newOutput)
            case _: Failed => newState
            case _ => runAmplifiers(newQueue.enqueue(newState.copy(output = Nil)), newOutput)
        }
    }
}
