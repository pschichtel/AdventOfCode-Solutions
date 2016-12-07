package tel.schich.adventofcode

import java.io.FileNotFoundException
import java.util.concurrent.TimeUnit

import scala.concurrent.duration.TimeUnit
import scala.io.Source

trait AoCApp extends App {
    def sourceFromCP(path: String): Source = {
        val resourceStream = getClass.getResourceAsStream(path)
        if (resourceStream == null) {
            throw new FileNotFoundException(s"File not found in classpath: $path")
        }
        Source.fromInputStream(resourceStream)
    }

    lazy val inputSource = sourceFromCP("/" + this.getClass.getName.replace("$", "").replaceAll("\\.", "/") + ".txt")
    def inputText: String = inputSource.mkString.trim
    def inputLines: Seq[String] = inputSource.getLines().map(_.trim).filter(_.nonEmpty).toSeq

    def part(n: Int, value: Any): Unit = {
        println(s"Part $n: $value")
    }

    def notImplementedYet(): Unit = {
        println("Not implemented yet!")
        System.exit(1)
    }

    def timed[U](unit: TimeUnit)(value: => U): U = {
        val start = System.currentTimeMillis()
        val delta = unit.convert(System.currentTimeMillis() - start, TimeUnit.MILLISECONDS)
        println(s"Time: $delta ms")
        value
    }
}
