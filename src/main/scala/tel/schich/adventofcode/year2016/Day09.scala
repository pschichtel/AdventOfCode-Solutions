package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.AoCApp

import scala.annotation.tailrec

object Day09 extends AoCApp {

    def countCharsDecompressed(input: String, recurse: Boolean = false): Long = {

        @tailrec
        def countCompressedSeqs(input: List[Char], count: Long): Long = {
            val (seqRead, length) = countCompressedSeq(input)
            val newLength = count + length
            if (seqRead.nonEmpty) countCompressedSeqs(seqRead, newLength)
            else newLength
        }

        def countCompressedSeq(input: List[Char]): (List[Char], Long) = {
            val (prefixRead, prefixLength) = countCharSeq(input, 0)
            if (prefixRead.nonEmpty) {
                val (headRead, length, repetitions) = readCompressionHead(prefixRead)
                val repRead = headRead.drop(length)
                val resolvedLength =
                    if (recurse) countCompressedSeqs(headRead.take(length), 0)
                    else length
                (repRead, prefixLength + resolvedLength * repetitions)

            } else (prefixRead, prefixLength)
        }

        def readCompressionHead(input: List[Char]): (List[Char], Int, Int) = {
            val (next, length) = readNumber(input.drop(1), "")
            val (end, repetitions) = readNumber(next.drop(1), "")
            (end.drop(1), length, repetitions)
        }

        @tailrec
        def readNumber(input: List[Char], number: String): (List[Char], Int) = {
            if (input.nonEmpty && Character.isDigit(input.head)) readNumber(input.tail, number + input.head)
            else (input, number.toInt)
        }

        @tailrec
        def countCharSeq(input: List[Char], count: Int): (List[Char], Long) = {
            if (input.nonEmpty && input.head != '(') countCharSeq(input.tail, count + 1)
            else (input, count)
        }

        countCompressedSeqs(input.toList, 0)

    }

    val strippedInput = inputText.replaceAll("\\s+", "")

    part(1, countCharsDecompressed(strippedInput))
    part(2, countCharsDecompressed(strippedInput, recurse = true))
}
