package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.AoCApp

import scala.collection.immutable.ArraySeq

object Day02 extends AoCApp {
    val ExitPosition = -1
    type Program = ArraySeq[Int]
    type Instruction = (Program, Int) => (Program, Int)

    def binaryOp(op: (Int, Int) => Int)(mem: Program, pc: Int): (Program, Int) = {
        val a = mem(pc + 1)
        val b = mem(pc + 2)
        (mem.updated(pc + 3, op(a, b)), pc + 4)
    }

    def interpret(instructions: Map[Int, Instruction])(mem: Program, pc: Int): Program = {
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

    val program: Program = ArraySeq.unsafeWrapArray(inputText.split(',').map(_.toInt))
    val programAlertState = patch(program, 1 -> 12, 2 -> 2)
    val resultMem = interpret(instructions)(program, 0)

    part(1, resultMem(0))
}
