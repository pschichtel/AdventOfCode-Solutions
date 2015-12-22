import scala.math._
import scala.annotation.tailrec
import scala.collection.immutable.Queue

println("Day 10")

val input = "1113122113"
val iterations = (40, 50)

@tailrec
def getSpans(s: String, spans: Queue[String] = Queue()): Seq[String] = {
    if (s.isEmpty) spans
    else {
        val (span, rest) = s.span(_ == s(0))
        getSpans(rest, spans :+ span)
    }
}

def transform(s: String): String = {
    getSpans(s).map(s => s.length.toString + s(0)).mkString
}

def transformer(input: String, it: Int) = {
    //println(it + 1)
    transform(input)
}

val transformed = (0 until iterations._1).foldLeft(input)(transformer)
println(s"Part 1: ${transformed.length}")

val transformedMore = (0 until +(iterations._2 - iterations._1)).foldLeft(transformed)(transformer)
println(s"Part 2: ${transformedMore.length}")
