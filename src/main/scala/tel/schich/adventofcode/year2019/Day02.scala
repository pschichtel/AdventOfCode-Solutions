package tel.schich.adventofcode.year2019

import java.util.concurrent.TimeUnit

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day02 extends AoCApp {
    val ExitPosition = -1
    type Program = ArraySeq[Int]
    type Instruction = (Program, Int) => (Program, Int)
    type InstructionSet = Map[Int, Instruction]

    def deRef(mem: Program, pointerPointer: Int): Int = mem(mem(pointerPointer))

    def binaryOp(op: (Int, Int) => Int)(mem: Program, pc: Int): (Program, Int) = {
        val a = deRef(mem, pc + 1)
        val b = deRef(mem, pc + 2)
        (mem.updated(mem(pc + 3), op(a, b)), pc + 4)
    }

    @tailrec
    def interpret(instructions: InstructionSet, mem: Program, pc: Int): Program = {
        if (pc == ExitPosition) mem
        else {
            val (newMem, newPc) = instructions(mem(pc))(mem, pc)
            interpret(instructions, newMem, newPc)
        }
    }

    def patch(mem: Program, patches: (Int, Int)*): Program =
        patches.foldLeft(mem)((c, p) => c.updated(p._1, p._2))

    def exit(mem: Program, pc: Int): (Program, Int) = (mem, ExitPosition)

    timed(TimeUnit.MICROSECONDS) {
        val instructions: Map[Int, Instruction] = Map(
            1 -> binaryOp(_ + _),
            2 -> binaryOp(_ * _),
            99 -> exit
        )


        val program: Program = splitInput(',').map(_.toInt)
        val invoke = (a: Int, b: Int) => interpret(instructions, patch(program, 1 -> a, 2 -> b), 0)(0)
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
