package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day23._

object Day12 extends AoCApp {

    def loadProgram(rawInstrctions: Seq[String]): Array[Instruction] = {
        val cpyRegister = "cpy ([a-zA-Z]+) (\\w+)".r
        val cpyConstant = "cpy (\\d+) (\\w+)".r
        val inc = "inc (\\w+)".r
        val dec = "dec (\\w+)".r
        val jnzRegister = "jnz ([a-zA-Z]+) ([+-]?\\d+)".r
        val jnzConstant = "jnz (\\d+) ([+-]?\\d+)".r

        rawInstrctions.map {
            case cpyRegister(source, target) => Mutation(target, (cpu, r) => cpu.registerValue(source))
            case cpyConstant(value, target) => Mutation(target, (_, r) => value.toInt)
            case inc(target) => Mutation(target, (_, r) => r + 1)
            case dec(target) => Mutation(target, (_, r) => r - 1)
            case jnzRegister(r, offset) => Jump(_.registerValue(r) != 0, offset.toInt)
            case jnzConstant(value, offset) => Jump(_ => value.toInt != 0, offset.toInt)
        }.toArray
    }


    val program = loadProgram(inputLines)

    val brokenCpu = execute(Processor(Map.empty, 0), program)
    part(1, brokenCpu.registerValue("a"))

    val correctCpu = execute(Processor(Map("c" -> 1), 0), program)
    part(1, correctCpu.registerValue("a"))
}
