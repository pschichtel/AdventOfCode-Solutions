package tel.schich.adventofcode.shared

import java.io.FileNotFoundException
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq
import scala.io.Source

trait AoCApp extends App {

    val name = getClass.getSimpleName
    lazy val printStuff = !args.contains("silent")

    def sourceFromCP(path: String): Source = {
        val resourceStream = getClass.getResourceAsStream(path)
        if (resourceStream == null) {
            throw new FileNotFoundException(s"File not found in classpath: $path")
        }
        Source.fromInputStream(resourceStream)
    }

    lazy val inputSource = sourceFromCP("/" + this.getClass.getName.replace("$", "").replaceAll("\\.", "/") + ".txt")

    def input[T](implicit parser: Parser[T]): T = {
        parser(inputText) match {
            case ParseResult.Success(value, _) => value
            case ParseResult.Error(error, rest) =>
                println(s"Failed to parse input:")
                error.printStackTrace(System.err)
                println(s"Remaining input:\n$rest")
                System.exit(1)
                throw new Exception()
        }
    }

    lazy val inputText: String = inputSource.mkString.trim

    private lazy val lines: Array[String] = inputSource.getLines().map(_.trim).filter(_.nonEmpty).toArray

    def inputLines: Seq[String] = lines

    def splitInput(at: String) = ArraySeq.unsafeWrapArray(inputText.split(at))

    def splitInput(at: Char) = ArraySeq.unsafeWrapArray(inputText.split(at))

    def part(n: Int, value: Any): Unit = {
        if (printStuff) {
            println(s"Part $n: $value")
        }
    }

    def notImplementedYet(): Unit = {
        println("Not implemented yet!")
        System.exit(1)
    }

    def timed[U](unit: TimeUnit)(value: => U): U = {
        val start = System.nanoTime()
        val result = value
        val delta = unit.convert(System.nanoTime() - start, TimeUnit.NANOSECONDS)
        println(s"Time: $delta µs")
        result
    }
}
