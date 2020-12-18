package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day04 extends AoCApp {

    private val values = "((?:\\w+-)+)(\\d+)\\[(\\w+)]".r
    private val alphabet: Vector[Char] = (0 until 26).map (i => ('a' + i).toChar).toVector

    def frequencies(string: String): Seq[(Char, Int)] = string.toSeq.groupBy(identity).view.mapValues(_.length).toSeq
    def decrypt(string: String, shift: Int): String = string.map(c => alphabet((c - 'a' + shift) % alphabet.length)).mkString


    override def solution: (Any, Any) = {
        val correct = asLines(Input2016.Day04).flatMap {
            case values(string, id, checksum) =>
                val encryptedWords = string.split("-")
                val selectorId = id.toInt
                val freqs = frequencies(encryptedWords.mkString)

                // expects a stable sort algorithm
                if (freqs.sortBy(_._1).sortBy(-_._2).take(5).map(_._1).mkString != checksum) None
                else Some(selectorId, encryptedWords.map(decrypt(_, selectorId)).mkString(" "))
        }

        val part1 = correct.map(_._1).sum

        val Some((northPoleStorageId, _)) = correct.find(_._2 == "northpole object storage")
        val part2 = northPoleStorageId

        (part1, part2)
    }
}
