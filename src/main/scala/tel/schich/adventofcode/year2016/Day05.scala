package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day04.md5

import scala.annotation.tailrec

object Day05 extends AoCApp {

    val input = "cxdnnyjw"

    def findPassword[A](input: String, acc: A, fold: (A, String) => A, done: A => Boolean): A = {

        @tailrec
        def recurse(out: A, i: Int): A = {
            if (done(out)) out
            else recurse(fold(out, md5(input + i)), i + 1)
        }

        recurse(acc, 0)
    }

    def interesting(hash: String): Boolean = hash.startsWith("00000")

    val firstDoorPassword: String = findPassword(
        input, "",
        (pass: String, s: String) => if (interesting(s)) pass + s(5) else pass,
        (s: String) => s.length == 8
    )

    part(1, firstDoorPassword)

    def isCodePosition(c: Char): Boolean = c >= '0' && c <= '7'

    val secondDoorPassword: String = findPassword(
        input, "________",
        (pass: String, s: String) => {
            if (interesting(s) && isCodePosition(s(5))) {
                val pos = s(5) - '0'
                if (pass(pos) == '_') {
                    val newPass = pass.updated(pos, s(6))
                    println(s"$s -> $newPass")
                    newPass
                } else pass
            } else pass
        },
        (s: String) => !s.contains('_')
    )

    part(2, secondDoorPassword)
}
