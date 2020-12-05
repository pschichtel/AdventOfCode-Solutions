package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.year2015.Day04.hash
import tel.schich.adventofcode.year2015.Day04.startsWithNZeros

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day05 extends AoCApp {

    def findPassword[A](input: String, acc: A, fold: (A, ArraySeq[Byte]) => A, done: A => Boolean): A = {

        val md5 = hash("MD5")

        @tailrec
        def recurse(out: A, i: Int): A = {
            if (done(out)) out
            else recurse(fold(out, md5(input + i)), i + 1)
        }

        recurse(acc, 0)
    }

    def binaryToHex(bin: Seq[Byte]): Seq[Char] = {
        bin.flatMap("%02X".format(_))
    }

    val input = inputText
    val interesting = startsWithNZeros(5)

    val firstDoorPassword: String = findPassword(
        input, "",
        (pass: String, binHash: ArraySeq[Byte]) => {
            if (interesting(binHash)) {
                val hex = binaryToHex(binHash)
                pass + hex(5)
            } else pass
        },
        (s: String) => s.length == 8
    )

    part(1, firstDoorPassword)

    def isCodePosition(c: Char): Boolean = c >= '0' && c <= '7'

    val secondDoorPassword: String = findPassword(
        input, "________",
        (pass: String, bin: ArraySeq[Byte]) => {
            if (interesting(bin)) {
                val hex = binaryToHex(bin)
                if (isCodePosition(hex(5))) {
                    val pos = hex(5) - '0'
                    if (pass(pos) == '_') {
                        val newPass = pass.updated(pos, hex(6))
                        println(s"${hex.mkString} -> $newPass")
                        newPass
                    } else pass
                } else pass
            } else pass
        },
        (s: String) => !s.contains('_')
    )

    part(2, secondDoorPassword)
}
