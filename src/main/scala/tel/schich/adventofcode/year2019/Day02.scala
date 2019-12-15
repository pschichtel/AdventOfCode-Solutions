package tel.schich.adventofcode.year2019

import java.util.concurrent.TimeUnit

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day02 extends AoCApp {
    type Program = ArraySeq[Int]
    type Input = List[Int]
    type Output = List[Int]
    type Instruction = ProgramState => ProgramState
    type InstructionSet = Map[Int, Instruction]

    sealed trait _ProgramState {
        def instructionSet: InstructionSet
        def program: Program
        def pc: Int
        def input: Input
        def output: Output
    }
    sealed trait Status
    case object SuccessfullyCompleted extends Status
    case object RequiredMoreInput extends Status
    case object Ready extends Status
    case class Failed(e: Exception) extends Status

    case class ProgramState(instructionSet: InstructionSet, program: Program, pc: Int, input: Input, output: Output, status: Status) {

        lazy val instruction = instructionSet(program(pc) % 100)

        def runInstruction(): ProgramState = {
            try {
                instruction(this)
            } catch {
                case e: Exception => copy(status = Failed(e))
            }
        }

        def continue(program: Program = this.program, pc: Int = this.pc, input: Input = this.input, output: Output = this.output): ProgramState =
            this.copy(program = program, pc = pc, input = input, output = output, status = Ready)

        def complete(program: Program = this.program, pc: Int = this.pc, input: Input = this.input, output: Output = this.output): ProgramState =
            this.copy(program = program, pc = pc, input = input, output = output, status = SuccessfullyCompleted)

        def requireInput(): ProgramState =
            this.copy(status = RequiredMoreInput)

        def readParam(offset: Int): Int = {
            val opCode = program(pc)
            val value = program(pc + offset)
            val mode = (opCode / math.pow(10, 1 + offset).toInt) % 10

            mode match {
                case 0 => program(value)
                case 1 => value
            }
        }
    }

    def parseProgram(input: String): Program = ArraySeq.unsafeWrapArray(input.split(',')).map(_.toInt)

    def initProgram(instructionSet: InstructionSet, program: Program, input: List[Int]): ProgramState =
        ProgramState(instructionSet, program, 0, input, Nil, Ready)

    def runProgram(instructionSet: InstructionSet, program: Program, input: List[Int] = Nil): ProgramState =
        runProgram(initProgram(instructionSet, program, input))

    def runProgram(startState: ProgramState): ProgramState = {
        @tailrec
        def interpret(state: ProgramState): ProgramState = {
            state.status match {
                case Ready => interpret(state.runInstruction())
                case SuccessfullyCompleted => state
                case RequiredMoreInput => state
                case Failed(e) => {
                    e.printStackTrace(System.err)
                    state
                }
            }
        }

        interpret(startState)
    }

    def patch(mem: Program, patches: (Int, Int)*): Program =
        patches.foldLeft(mem)((c, p) => c.updated(p._1, p._2))

    def binaryOp(op: (Int, Int) => Int)(state: ProgramState): ProgramState =
        state.continue(
            program = state.program.updated(state.program(state.pc + 3), op(state.readParam(1), state.readParam(2))),
            pc = state.pc + 4
        )

    def comparisonOp(comp: (Int, Int) => Boolean)(state: ProgramState) =
        binaryOp((a, b) => if (comp(a, b)) 1 else 0)(state)

    def exit(state: ProgramState) =
        state.complete()

    def readInput(state: ProgramState) =
        state.input match {
            case Nil => state.requireInput()
            case head :: tail => state.continue(
                program = state.program.updated(state.program(state.pc + 1), head),
                pc = state.pc + 2,
                input = tail
            )
        }

    def writeOutput(state: ProgramState): ProgramState =
        state.continue(
            pc = state.pc + 2,
            output = state.output :+ state.readParam(1)
        )

    def jumpIf(condition: Int => Boolean)(state: ProgramState) =
        if (condition(state.readParam(1))) state.continue(pc = state.readParam(2))
        else state.continue(pc = state.pc + 3)

    lazy val instructions: Map[Int, Instruction] = Map(
        1 -> binaryOp(_ + _),
        2 -> binaryOp(_ * _),
        3 -> readInput,
        4 -> writeOutput,
        99 -> exit
    )

    timed(TimeUnit.MICROSECONDS) {
        val program = parseProgram(inputText)
        val invoke = (a: Int, b: Int) => runProgram(instructions, patch(program, 1 -> a, 2 -> b), List.empty).program(0)
        part(1, invoke(12, 2))

        part(2, {
            val requiredOutput = 19690720
            val solutions = for {
                noun <- 0 to 99
                verb <- 0 to 99
                if invoke(noun, verb) == requiredOutput
            } yield 100 * noun + verb
            solutions.head
        })
    }

}
