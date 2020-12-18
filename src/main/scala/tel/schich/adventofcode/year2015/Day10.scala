package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day10 extends AoCApp {

    private val iterations = (40, 50)

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

    def transformer(input: VString, it: Int): VString = lookAndSay(input)

    override def solution: (Any, Any) = {
        val input = vstr(Input2015.Day10)
        val transformed = (0 until iterations._1).foldLeft(input)(transformer)
        val transformedMore = (iterations._1 until iterations._2).foldLeft(transformed)(transformer)

        (transformed.length, transformedMore.length)
    }
}
