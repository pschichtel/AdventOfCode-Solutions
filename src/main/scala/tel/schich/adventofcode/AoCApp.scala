package tel.schich.adventofcode

import scala.io.Source

trait AoCApp extends App {
    def sourceFromCP(path: String): Source = Source.fromInputStream(getClass.getResourceAsStream(path))

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
}
