import scala.annotation.tailrec

object Day08 extends AoCApp {
    println("Day  8")

    val input = sourceFromCP("day08.txt").mkString.trim.split('\n').map(_.trim).toSeq

    def parse(s: String): String = parseChars(s.iterator)

    @tailrec
    private def parseChars(s: Iterator[Char], acc: String = ""): String = {
        if (s.hasNext) parseChars(s, acc + parseChar(s))
        else acc
    }

    def parseChar(s: Iterator[Char]): String = {
        s.next match {
            case '\\' => parseEscapeSequence(s)
            case '"' => ""
            case c => "" + c
        }
    }

    def parseEscapeSequence(s: Iterator[Char]): String = {
        s.next match {
            case '"' => "\""
            case '\\' => "\\"
            case 'x' =>
                "" + Integer.parseInt("" + s.next + s.next, 16).toChar
            case c => "\\" + c
        }
    }

    def sizes(strings: Seq[(String, String)]) =
        strings.map { case (ext, int) => (ext.length, int.length) }
            .foldLeft((0, 0)) { case ((aext, aint), (bext, bint)) => (aext + bext, aint + bint) }

    val parsedStrings = input.map(s => (s, parse(s)))
    val (ext, int) = sizes(parsedStrings)

    println(s"Part 1: ${ext - int}")


    def encode(s: String) = {

        '"' + s.flatMap {
            case '"' => "\\\""
            case '\\' => "\\\\"
            case c => "" + c
        } + '"'

    }

    val encodedStrings = input.map(s => (s, encode(s)))
    val (normal, encoded) = sizes(encodedStrings)

    println(s"Part 2: ${encoded - normal}")

}
