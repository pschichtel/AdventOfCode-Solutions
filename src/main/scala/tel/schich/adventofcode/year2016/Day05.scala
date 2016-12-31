package tel.schich.adventofcode.year2016

import java.security.MessageDigest

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day04.hash
import tel.schich.adventofcode.year2015.Day04.startsWithNZeros

import scala.annotation.tailrec

object Day05 extends AoCApp {

    val md5 = hash(MessageDigest.getInstance("MD5"), _: String)

    def findPassword[A](input: String, acc: A, fold: (A, Array[Byte]) => A, done: A => Boolean): A = {

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
        (pass: String, binHash: Array[Byte]) => {
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
        (pass: String, bin: Array[Byte]) => {
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
