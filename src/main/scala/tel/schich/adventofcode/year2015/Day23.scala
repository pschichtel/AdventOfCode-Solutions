package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day23 extends AoCApp {

    sealed trait Instruction

    case class Mutation(r: String, f: (Processor, Int) => Int) extends Instruction
    case class Jump(condition: Processor => Boolean, offset: Int) extends Instruction

    case class Processor(registers: Map[String, Int], pc: Int) {
        def isRegister(r: String, f: Int => Boolean): Boolean = f(registers.getOrElse(r, 0))
        def registerValue(r: String): Int = registers.getOrElse(r, 0)
        def applyToRegister(r: String, f: (Processor, Int) => Int): Processor = {
            copy(registers = registers.updated(r, f(this, registerValue(r))))
        }

        def goto(to: Int): Processor = copy(pc = pc + to)
        def nextInstruction: Processor = goto(1)
    }

    @tailrec
    def execute(cpu: Processor, program: Array[Instruction]): Processor = {
        if (!program.isDefinedAt(cpu.pc)) cpu
        else {
            val instruction = program(cpu.pc)

            instruction match {
                case Mutation(r, f) =>
                    execute(cpu.applyToRegister(r, f).nextInstruction, program)
                case Jump(condition, to) =>
                    if (condition(cpu)) execute(cpu.goto(to), program)
                    else execute(cpu.nextInstruction, program)
            }
        }

    }

    def loadProgram(rawInstructions: Seq[String]): Array[Instruction] = {
        val hlf = "hlf (\\w+)".r
        val tpl = "tpl (\\w+)".r
        val inc = "inc (\\w+)".r
        val jmp = "jmp ([+-]\\d+)".r
        val jie = "jie (\\w+), ([+-]\\d+)".r
        val jio = "jio (\\w+), ([+-]\\d+)".r

        rawInstructions.map {
            case hlf(r) => Mutation(r, (_, r) => r / 2)
            case tpl(r) => Mutation(r, (_, r) => r * 3)
            case inc(r) => Mutation(r, (_, r) => r + 1)
            case jmp(offset) => Jump(_ => true, offset.toInt)
            case jie(r, offset) => Jump(_.registerValue(r) % 2 == 0, offset.toInt)
            case jio(r, offset) => Jump(_.registerValue(r) == 1, offset.toInt)
        }.toArray
    }

    override def solution: (Any, Any) = {
        val program = loadProgram(asLines(Input2015.Day23))

        val finalState = execute(Processor(Map(), 0), program)
        val part1 = finalState.registerValue("b")

        val secondFinalState = execute(Processor(Map("a" -> 1), 0), program)
        val part2 = secondFinalState.registerValue("b")

        (part1, part2)
    }
}
