package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.year2015.Day23._

object Day12 extends AoCApp {

    def loadProgram(rawInstructions: Seq[String]): Array[Instruction] = {
        val cpyRegister = "cpy ([a-zA-Z]+) (\\w+)".r
        val cpyConstant = "cpy (\\d+) (\\w+)".r
        val inc = "inc (\\w+)".r
        val dec = "dec (\\w+)".r
        val jnzRegister = "jnz ([a-zA-Z]+) ([+-]?\\d+)".r
        val jnzConstant = "jnz (\\d+) ([+-]?\\d+)".r

        rawInstructions.map {
            case cpyRegister(source, target) => Mutation(target, (cpu, _) => cpu.registerValue(source))
            case cpyConstant(value, target) => Mutation(target, (_, _) => value.toInt)
            case inc(target) => Mutation(target, (_, r) => r + 1)
            case dec(target) => Mutation(target, (_, r) => r - 1)
            case jnzRegister(r, offset) => Jump(_.registerValue(r) != 0, offset.toInt)
            case jnzConstant(value, offset) => Jump(_ => value.toInt != 0, offset.toInt)
        }.toArray
    }


    override def solution: (Any, Any) = {
        val program = loadProgram(asLines(Input2016.Day12))

        val brokenCpu = execute(Processor(Map.empty, 0), program)
        val part1 = brokenCpu.registerValue("a")

        val correctCpu = execute(Processor(Map("c" -> 1), 0), program)
        val part2 = correctCpu.registerValue("a")

        (part1, part2)
    }
}
