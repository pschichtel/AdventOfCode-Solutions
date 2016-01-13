import scala.collection.immutable.Queue
import scala.math._
import scala.annotation.tailrec

object Day10 extends AoCApp {
    println("Day 10")

    val input = "1113122113"
    val iterations = (40, 50)

    @inline
    private def toC(i: Int) = ('0' + i).toChar
    @inline
    private def toS(i: Int) = toC(i).toString

    @tailrec
    private def lookAndSay(s: String, offset: Int = 1, seqLength: Int = 1, said: Queue[String] = Queue()): String = {
        if (offset >= s.length) said.mkString + toC(seqLength) + s.last
        else {
            val a = s(offset - 1)
            val b = s(offset)
            if (a == b) lookAndSay(s, offset + 1, seqLength + 1, said)
            else lookAndSay(s, offset + 1, 1, said :+ (toS(seqLength) + a))
        }
    }

    def transformer(input: String, it: Int) = lookAndSay(input)

    // warmup
    //(0 until iterations._2).foldLeft(input)(transformer)

    val transformed = (0 until iterations._1).foldLeft(input)(transformer)
    println(s"Part 1: ${transformed.length}")

    val transformedMore = (iterations._1 until iterations._2).foldLeft(transformed)(transformer)
    println(s"Part 2: ${transformedMore.length}")
}