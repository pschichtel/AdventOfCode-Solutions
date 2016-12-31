package tel.schich.adventofcode.year2015

import java.security.MessageDigest

import tel.schich.adventofcode.AoCApp

object Day04 extends AoCApp {

    def hash(function: String): String => Array[Byte] = {
        val hashFunction = MessageDigest.getInstance(function)
        (s: String) => hashFunction.digest(s.getBytes)
    }

    val md5 = hash("MD5")

    def startsWithNZeros(n: Int): Array[Byte] => Boolean = {
        val fullBytes = n / 2
        val halfByteMask = if (fullBytes < n) 0xF0 else 0xFF

        (hash: Array[Byte]) => {
            (hash.view(0, fullBytes).foldLeft(0)((a, b) => a + Math.abs(b)) + (hash(fullBytes) & halfByteMask)) == 0
        }
    }

    def hashStream(input: String, number: Int = 0): Stream[(Int, Array[Byte])] = (number, md5(input + number)) #:: hashStream(input, number + 1)

    val input = inputText

    private val startsWith5Zeros = startsWithNZeros(5)
    var (n1, _) = hashStream(input).filter(p => startsWith5Zeros(p._2)).head
    println(s"Part 1: $n1")

    private val startsWith6Zeros = startsWithNZeros(6)
    val (n2, _) = hashStream(input).filter(p => startsWith6Zeros(p._2)).head
    println(s"Part 2: $n2")
}
