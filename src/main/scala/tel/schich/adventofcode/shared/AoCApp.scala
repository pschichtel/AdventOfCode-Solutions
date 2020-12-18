package tel.schich.adventofcode.shared

import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

abstract class AoCApp {

    private var args: Array[String] = Array.empty

    val Name: String = getClass.getSimpleName.replace("$", "")
    private val printStuff = !args.contains("silent")

    def splitInput(input: String, at: Char): ArraySeq[String] = ArraySeq.unsafeWrapArray(input.split(at))

    private def part(n: Int, value: Any): Unit = {
        if (printStuff) {
            println(s"Part $n: $value")
        }
    }

    def asLines(input: String): ArraySeq[String] = ArraySeq.unsafeWrapArray(input.split("\r?\n"))
        .map(_.trim)
        .filter(_.nonEmpty)

    def parse[T](input: String, parser: Parser[T]): T =  timed(s"$Name input parsing") {
        parser(StringSlice(input)) match {
            case ParseResult.Success(value, rest) =>
                if (rest.length == 0) value
                else {
                    println("Parsing succesful, but incomplete!")
                    println(s"Parsed: $value")
                    println(s"Rest:\n${rest.asString}")
                    throw new Exception("incomplete!")
                }
            case ParseResult.Error(error, rest) =>
                println(s"Failed to parse input:")
                println(s"Remaining input:\n${rest.asString}")
                throw new Exception(error)
        }
    }

    def timed[U](label: String)(value: => U): U = {
        val start = System.nanoTime()
        val result = value
        val delta = TimeUnit.MICROSECONDS.convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
        if (printStuff) {
            println(s"$label: $delta µs")
        }
        result
    }


    def main(args: Array[String]): Unit = {
        this.args = args

        val (a, b) = timed(s"$Name solve time") {
            solution
        }

        part(1, a)
        part(2, b)
    }

    final val Unsolved: String = "unsolved"

    def solution: (Any, Any) = (Unsolved, Unsolved)
}
