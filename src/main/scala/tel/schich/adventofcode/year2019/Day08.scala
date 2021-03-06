package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day08 extends AoCApp {

    private val width = 25
    private val height = 6
    private val size = width * height

    private val blankImage = ("2" * size).toSeq

    @tailrec
    def resolveImage(input: String, image: IndexedSeq[Char], offset: Int): IndexedSeq[Char] = {
        if (offset >= input.length) image
        else {
            val newImage = for (i <- image.indices) yield {
                val inputOffset = offset + i
                if (input(inputOffset) != '2' && image(i) == '2') input(inputOffset)
                else image(i)
            }
            resolveImage(input, newImage, offset + image.length)
        }
    }

    override def solution: (Any, Any) = {
        val input = Input2019.Day08

        val checkSum = input.grouped(size)
            .toSeq
            .sortBy(_.count(_ == '0'))
            .map(l => l.count(_ == '1') * l.count(_ == '2'))
            .head

        val part1 = checkSum

        val resolved = resolveImage(input, blankImage, 0).mkString

        val part2 = resolved.map {
            case '0' => ' '
            case '1' => '#'
        }.grouped(width).mkString("\n", "\n", "")

        (part1, part2)
    }
}
