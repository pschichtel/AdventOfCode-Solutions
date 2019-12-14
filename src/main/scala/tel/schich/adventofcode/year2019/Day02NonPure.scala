package tel.schich.adventofcode.year2019

import java.util.concurrent.TimeUnit

import tel.schich.adventofcode.AoCApp

object Day02NonPure extends AoCApp {
    timed(TimeUnit.MICROSECONDS) {
        val input = inputText
        val inputLen = input.length
        val program = Array.ofDim[Int](input.length)

        var inputIndex = 0
        var programLength = 0
        var currentInstruction = 0
        var c: Char = 'a'

        while (inputIndex < inputLen) {
            c = input.charAt(inputIndex)
            inputIndex += 1
            if (c == ',') {
                program(programLength) = currentInstruction
                programLength += 1
                currentInstruction = 0
            } else {
                currentInstruction = currentInstruction * 10 + (c - '0')
            }
        }
        program(programLength) = currentInstruction
        programLength += 1

        val runCopy = Array.ofDim[Int](programLength)

        program.copyToArray(runCopy, 0, programLength)
        runCopy(1) = 12
        runCopy(2) = 2
        run(runCopy)
        part(1, runCopy(0))

        part(2, solve())

        def solve(): Int = {
            var noun = 0
            var verb = 0
            while (noun < 100) {
                while (verb < 100) {
                    program.copyToArray(runCopy, 0, programLength)
                    runCopy(1) = noun
                    runCopy(2) = verb
                    run(runCopy)
                    if (runCopy(0) == 19690720) {
                        return 100 * noun + verb
                    }
                    verb += 1
                }
                verb = 0
                noun += 1
            }
            -1
        }
    }


    @inline
    def run(program: Array[Int]): Unit = {
        var pc = 0
        val len = program.length
        while (pc < len) {
            program(pc) match {
                case 1 =>
                    program(program(pc + 3)) = program(program(pc + 1)) + program(program(pc + 2))
                    pc += 4
                case 2 =>
                    program(program(pc + 3)) = program(program(pc + 1)) * program(program(pc + 2))
                    pc += 4
                case 99 => return
            }
        }
    }

//    val program: Program = splitInput(',').map(_.toInt)
//    val invoke = (a: Int, b: Int) => interpret(instructions, patch(program, 1 -> a, 2 -> b), 0)(0)
//
//    part(1, invoke(12, 2))
//
//    part(2, timed(TimeUnit.MILLISECONDS) {
//        val requiredOutput = 19690720
//        val solutions = for {
//            noun <- 0 to 99
//            verb <- 0 to 99
//            if invoke(noun, verb) == requiredOutput
//        } yield 100 * noun + verb
//        solutions.head
//    })

}
