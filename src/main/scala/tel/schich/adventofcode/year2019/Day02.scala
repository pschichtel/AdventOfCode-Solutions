package tel.schich.adventofcode.year2019

import java.util.concurrent.TimeUnit

import tel.schich.adventofcode.AoCApp

import scala.collection.immutable.ArraySeq

object Day02 extends AoCApp {
    val ExitPosition = -1
    type Program = ArraySeq[Int]
    type Instruction = (Program, Int) => (Program, Int)
    type InstructionSet = Map[Int, Instruction]

    def binaryOp(op: (Int, Int) => Int)(mem: Program, pc: Int): (Program, Int) = {
        val a = mem(mem(pc + 1))
        val b = mem(mem(pc + 2))
        (mem.updated(mem(pc + 3), op(a, b)), pc + 4)
    }

    def interpret(instructions: InstructionSet)(mem: Program, pc: Int): Program = {
        if (pc == ExitPosition) mem
        else (interpret(instructions) _).tupled(instructions(mem(pc))(mem, pc))
    }

    def patch(mem: Program, patches: (Int, Int)*): Program =
        patches.foldLeft(mem)((c, p) => c.updated(p._1, p._2))


    val opAdd = binaryOp(_ + _) _
    val opMul = binaryOp(_ * _) _
    val opExit = (mem: Program, _: Int) => (mem, ExitPosition)

    val instructions: Map[Int, Instruction] = Map(
        1 -> opAdd,
        2 -> opMul,
        99 -> opExit
    )

    val program: Program = splitInput(',').map(_.toInt)

    val invoke = (a: Int, b: Int) => interpret(instructions)(patch(program, 1 -> a, 2 -> b), 0)(0)

    part(1, invoke(12, 2))

    part(2, timed(TimeUnit.MILLISECONDS) {
        val requiredOutput = 19690720
        val solutions = for {
            noun <- 0 to 99
            verb <- 0 to 99
            if invoke(noun, verb) == requiredOutput
        } yield 100 * noun + verb
        solutions.head
    })

}
