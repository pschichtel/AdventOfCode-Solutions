package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.AoCApp

object Day04 extends AoCApp {

    val Σ: Vector[Char] = (0 until 26).map (i => ('a' + i).toChar).toVector
    def frequencies(string: String): Seq[(Char, Int)] = string.groupBy(identity).mapValues(_.length).toSeq
    def decrypt(string: String, shift: Int): String = string.map(c => Σ((c - 'a' + shift) % Σ.length)).mkString

    val values = "((?:\\w+-)+)(\\d+)\\[(\\w+)\\]".r

    val correct = inputLines.flatMap {
        case values(string, id, checksum) =>
            val encryptedWords = string.split("-")
            val selectorId = id.toInt
            val freqs = frequencies(encryptedWords.mkString)

            // expects a stable sort algorithm
            if (freqs.sortBy(_._1).sortBy(-_._2).take(5).map(_._1).mkString != checksum) None
            else Some(selectorId, encryptedWords.map(decrypt(_, selectorId)).mkString(" "))
    }

    part(1, correct.map(_._1).sum)

    val Some((northPoleStorageId, _)) = correct.find(_._2 == "northpole object storage")
    part(2, northPoleStorageId)
}
