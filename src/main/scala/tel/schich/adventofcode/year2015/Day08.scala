package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day08 extends AoCApp {

    def parse(s: String): String = parseChars(s.iterator)

    @tailrec
    private def parseChars(s: Iterator[Char], acc: String = ""): String = {
        if (s.hasNext) parseChars(s, acc + parseChar(s))
        else acc
    }

    def parseChar(s: Iterator[Char]): String = {
        s.next() match {
            case '\\' => parseEscapeSequence(s)
            case '"' => ""
            case c => "" + c
        }
    }

    def parseEscapeSequence(s: Iterator[Char]): String = {
        s.next() match {
            case '"' => "\""
            case '\\' => "\\"
            case 'x' =>
                "" + Integer.parseInt("" + s.next() + s.next(), 16).toChar
            case c => "\\" + c
        }
    }

    def sizes(strings: Seq[(String, String)]): (Int, Int) =
        strings.map { case (ext, int) => (ext.length, int.length) }
            .foldLeft((0, 0)) { case ((aext, aint), (bext, bint)) => (aext + bext, aint + bint) }


    def encode(s: String): String = {

        "\"" + s.flatMap {
            case '"' => "\\\""
            case '\\' => "\\\\"
            case c => s"$c"
        } + "\""

    }

    override def solution: (Any, Any) = {

        val input = asLines(Input2015.Day08)

        val parsedStrings = input.map(s => (s, parse(s)))
        val (ext, int) = sizes(parsedStrings)

        val part1 = ext - int

        val encodedStrings = input.map(s => (s, encode(s)))
        val (normal, encoded) = sizes(encodedStrings)

        val part2 = encoded - normal

        (part1, part2)
    }
}
