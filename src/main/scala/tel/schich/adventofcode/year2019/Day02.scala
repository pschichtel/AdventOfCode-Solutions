package tel.schich.adventofcode.year2019

import java.util.concurrent.TimeUnit

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day02 extends AoCApp {
    final val ExitPosition = -1
    type Program = ArraySeq[Int]
    type Input = Seq[Int]
    type Output = Seq[Int]
    type Instruction = (Program, Int, Input, Output) => (Program, Int, Input, Output)
    type InstructionSet = Map[Int, Instruction]

    def parseProgram(input: String): Program = ArraySeq.unsafeWrapArray(input.split(',')).map(_.toInt)

    def runProgram(instructions: InstructionSet, mem: Program, in: Input): (Program, Output) = {
        @tailrec
        def interpret(mem: Program, pc: Int, in: Input, out: Output): (Program, Output) = {
            if (pc == ExitPosition) (mem, out)
            else {
                val instruction = instructions(mem(pc) % 100)
                val (newMem, newPc, newIn, newOut) = instruction(mem, pc, in, out)
                interpret(newMem, newPc, newIn, newOut)
            }
        }

        interpret(mem, 0, in, Vector.empty)
    }

    def readParam(mem: Program, pc: Int, param: Int): Int = {
        val opCode = mem(pc)
        val value = mem(pc + param)
        val mode = (opCode / math.pow(10, 1 + param).toInt) % 10

        mode match {
            case 0 => mem(value)
            case 1 => value
        }
    }

    def patch(mem: Program, patches: (Int, Int)*): Program =
        patches.foldLeft(mem)((c, p) => c.updated(p._1, p._2))

    def binaryOp(op: (Int, Int) => Int)(mem: Program, pc: Int, in: Input, out: Output) = {
        val a = readParam(mem, pc, 1)
        val b = readParam(mem, pc,  2)
        (mem.updated(mem(pc + 3), op(a, b)), pc + 4, in, out)
    }

    def comparisonOp(comp: (Int, Int) => Boolean)(mem: Program, pc: Int, in: Input, out: Output) =
        binaryOp((a, b) => if (comp(a, b)) 1 else 0)

    def exit(mem: Program, pc: Int, in: Input, out: Output) =
        (mem, ExitPosition, in, out)

    def readInput(mem: Program, pc: Int, in: Input, out: Output) =
        (mem.updated(mem(pc + 1), in.head), pc + 2, in.tail, out)

    def writeOutput(mem: Program, pc: Int, in: Input, out: Output) =
        (mem, pc + 2, in, out :+ readParam(mem, pc, 1))

    def jumpIf(condition: Int => Boolean)(mem: Program, pc: Int, in: Input, out: Output) =
        (mem, if (condition(readParam(mem, pc, 1))) readParam(mem, pc, 2) else pc + 3, in, out)

    lazy val instructions: Map[Int, Instruction] = Map(
        1 -> binaryOp(_ + _),
        2 -> binaryOp(_ * _),
        3 -> readInput,
        4 -> writeOutput,
        99 -> exit
    )

    timed(TimeUnit.MICROSECONDS) {
        val program = parseProgram(inputText)
        val invoke = (a: Int, b: Int) => runProgram(instructions, patch(program, 1 -> a, 2 -> b), List.empty)._1(0)
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
