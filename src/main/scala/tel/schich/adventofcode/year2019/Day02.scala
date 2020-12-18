package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day02 extends AoCApp {
    type Input = List[Long]
    type Output = List[Long]
    type Instruction = ProgramState => ProgramState
    type InstructionSet = Map[Long, Instruction]

    case class Memory(program: ArraySeq[Long], dynamic: Map[Long, Long]) {
        def apply(address: Long): Long = {
            if (address < 0) throw new IllegalArgumentException("illegal negative address")
            if (program.isDefinedAt(address.toInt)) program(address.toInt)
            else if (dynamic.isDefinedAt(address)) dynamic(address)
            else 0
        }

        def updated(address: Long, value: Long): Memory = {
            if (address < 0) throw new IllegalArgumentException("illegal negative address")
            else if (program.isDefinedAt(address.toInt)) this.copy(program = program.updated(address.toInt, value))
            else this.copy(dynamic = dynamic.updated(address, value))
        }
    }

    sealed trait Status
    case object SuccessfullyCompleted extends Status
    case object RequiredMoreInput extends Status
    case object Ready extends Status
    case class Failed(e: Exception) extends Status

    case class ProgramState(instructionSet: InstructionSet, memory: Memory, pc: Int, relativeBase: Long, input: Input, output: Output, status: Status) {

        private val MaxInstructions: Int = 100
        private val instruction: Instruction = instructionSet(memory.program(pc) % MaxInstructions)

        def runInstruction(): ProgramState = {
            try {
                instruction(this)
            } catch {
                case e: Exception => copy(status = Failed(e))
            }
        }

        def continue(memory: Memory = this.memory, pc: Int = this.pc, relativeBase: Long = this.relativeBase, input: Input = this.input, output: Output = this.output): ProgramState =
            this.copy(memory = memory, pc = pc, relativeBase = relativeBase, input = input, output = output, status = Ready)

        def complete(memory: Memory = this.memory, pc: Int = this.pc, relativeBase: Long = this.relativeBase, input: Input = this.input, output: Output = this.output): ProgramState =
            this.copy(memory = memory, pc = pc, relativeBase = relativeBase, input = input, output = output, status = SuccessfullyCompleted)

        def requireInput(): ProgramState =
            this.copy(status = RequiredMoreInput)

        private def resolveMode(offset: Long): (Long, Long) = {
            val opCode = memory(pc)
            val value = memory(pc + offset)

            (value, (opCode / math.pow(10, 1 + offset.toDouble).toInt) % 10)
        }

        def readParam(offset: Long): Long = {
            resolveMode(offset) match {
                case (addr, 0) => memory(addr)
                case (value, 1) => value
                case (relAddr, 2) => memory(relativeBase + relAddr)
                case (value, mode) => throw new IllegalArgumentException(s"unsupported input parameter mode for value $value at offset $offset: $mode")
            }
        }

        def writeMemoryTo(offset: Long, value: Long): Memory = {
            val absAddr = resolveMode(offset) match {
                case (addr, 0) => addr
                case (relAddr, 2) => relativeBase + relAddr
                case (value, mode) => throw new IllegalArgumentException(s"unsupported output parameter mode for value $value at offset $offset: $mode")
            }
            memory.updated(absAddr, value)
        }
    }

    def parseProgram(input: String): Memory = Memory(ArraySeq.unsafeWrapArray(input.split(',')).map(_.toLong), Map.empty)

    def initProgram(instructionSet: InstructionSet, program: Memory, input: Input): ProgramState =
        ProgramState(instructionSet, program, pc = 0, relativeBase = 0, input, output = Nil, status = Ready)

    def runProgram(instructionSet: InstructionSet, program: Memory, input: Input = Nil): ProgramState =
        runProgram(initProgram(instructionSet, program, input))

    def runProgram(startState: ProgramState): ProgramState = {
        @tailrec
        def interpret(state: ProgramState): ProgramState = {
            state.status match {
                case Ready => interpret(state.runInstruction())
                case SuccessfullyCompleted => state
                case RequiredMoreInput => state
                case Failed(e) =>
                    e.printStackTrace(System.err)
                    state
            }
        }

        interpret(startState)
    }

    def patch(mem: Memory, patches: (Long, Long)*): Memory =
        patches.foldLeft(mem)((c, p) => c.updated(p._1, p._2))

    def binaryOp(op: (Long, Long) => Long)(state: ProgramState): ProgramState =
        state.continue(
            memory = state.writeMemoryTo(3, op(state.readParam(1), state.readParam(2))),
            pc = state.pc + 4
        )

    def exit(state: ProgramState): ProgramState =
        state.complete()

    def readInput(state: ProgramState): ProgramState =
        state.input match {
            case Nil => state.requireInput()
            case head :: tail => state.continue(
                memory = state.writeMemoryTo(1, head),
                pc = state.pc + 2,
                input = tail
            )
        }

    def writeOutput(state: ProgramState): ProgramState =
        state.continue(
            pc = state.pc + 2,
            output = state.output :+ state.readParam(1)
        )

    lazy val instructions: InstructionSet = Map(
        1L -> binaryOp(_ + _),
        2L -> binaryOp(_ * _),
        3L -> readInput,
        4L -> writeOutput,
        99L -> exit
    )

    override def solution: (Any, Any) = {
        val program = parseProgram(Input2019.Day02)
        val invoke = (a: Long, b: Long) => runProgram(instructions, patch(program, 1L -> a, 2L -> b), List.empty).memory(0)

        val part1 = invoke(12, 2)

        val part2 = {
            val requiredOutput = 19690720
            val solutions = for {
                noun <- 0 to 99
                verb <- 0 to 99
                if invoke(noun, verb) == requiredOutput
            } yield 100 * noun + verb
            solutions.head
        }

        (part1, part2)
    }
}
