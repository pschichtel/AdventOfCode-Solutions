package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec

object Day10 extends AoCApp {
    println("Day 10")

    val input: VString = vstr("1113122113")
    val iterations = (40, 50)

    @inline
    private def toC(i: Int) = ('0' + i).toChar

    type VString = Vector[Char]

    @inline
    private def vstr(s: String): VString = Vector(s:_*)

    @tailrec
    private def lookAndSay(s: VString, offset: Int = 1, seqLength: Int = 1, said: VString = Vector.empty): VString = {
        if (offset >= s.length) said :+ toC(seqLength) :+ s.last
        else {
            val a = s(offset - 1)
            val b = s(offset)
            if (a == b) lookAndSay(s, offset + 1, seqLength + 1, said)
            else lookAndSay(s, offset + 1, 1, said :+ toC(seqLength) :+ a)
        }
    }

    def transformer(input: VString, it: Int) = lookAndSay(input)

    // warmup
    (0 until iterations._2).foldLeft(input)(transformer)

    val start = System.currentTimeMillis()
    val transformed = (0 until iterations._1).foldLeft(input)(transformer)
    println(s"Part 1: ${transformed.length} ${System.currentTimeMillis() - start}ms")

    val transformedMore = (iterations._1 until iterations._2).foldLeft(transformed)(transformer)
    println(s"Part 2: ${transformedMore.length} ${System.currentTimeMillis() - start}ms")
}
