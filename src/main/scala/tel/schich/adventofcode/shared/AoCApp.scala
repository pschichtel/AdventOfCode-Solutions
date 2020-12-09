package tel.schich.adventofcode.shared

import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

trait AoCApp extends App {

    val name = getClass.getSimpleName
    lazy val printStuff = !args.contains("silent")

    def splitInput(input: String, at: Char) = ArraySeq.unsafeWrapArray(input.split(at))

    def part(n: Int, value: Any): Unit = {
        if (printStuff) {
            println(s"Part $n: $value")
        }
    }

    def asLines(input: String) = ArraySeq.unsafeWrapArray(input.split("\r?\n"))
        .map(_.trim)
        .filter(_.nonEmpty)

    def parse[T](input: String, parser: Parser[T]): T = {
        parser(input) match {
            case ParseResult.Success(value, _) => value
            case ParseResult.Error(error, rest) =>
                println(s"Failed to parse input:")
                error.printStackTrace(System.err)
                println(s"Remaining input:\n$rest")
                throw error
        }
    }

    def notImplementedYet(): Unit = {
        throw new Exception("Not implemented yet!")
    }

    def timed[U](unit: TimeUnit)(value: => U): U = {
        val start = System.nanoTime()
        val result = value
        val delta = unit.convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
        println(s"Time: $delta µs")
        result
    }
}
