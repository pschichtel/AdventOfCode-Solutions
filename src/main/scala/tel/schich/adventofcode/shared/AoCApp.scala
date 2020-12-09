package tel.schich.adventofcode.shared

import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq

trait AoCApp extends App {

    val name = getClass.getSimpleName.replace("$", "")
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

    def parse[T](input: String, parser: Parser[T]): T =  timed(s"$name input parsing") {
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

    def notImplementedYet(): Unit = {
        throw new Exception("Not implemented yet!")
    }

    def timed[U](label: String)(value: => U): U = {
        val start = System.nanoTime()
        val result = value
        val delta = TimeUnit.MICROSECONDS.convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
        println(s"$label: $delta µs")
        result
    }
}
