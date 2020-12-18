package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import java.security.MessageDigest
import scala.collection.immutable.ArraySeq

object Day04 extends AoCApp {

    def hash(function: String): String => ArraySeq[Byte] = {
        val hashFunction = MessageDigest.getInstance(function)
        s => ArraySeq.unsafeWrapArray(hashFunction.digest(s.getBytes))
    }

    private val md5 = hash("MD5")

    def startsWithNZeros(n: Int): ArraySeq[Byte] => Boolean = {
        val fullBytes = n / 2
        val halfByteMask = if (fullBytes < n) 0xF0 else 0xFF

        (hash: ArraySeq[Byte]) => {
            (hash.slice(0, fullBytes).foldLeft(0)((a, b) => a + Math.abs(b)) + (hash(fullBytes) & halfByteMask)) == 0
        }
    }

    def hashStream(input: String, number: Int = 0): LazyList[(Int, ArraySeq[Byte])] = (number, md5(input + number)) #:: hashStream(input, number + 1)

    override def solution: (Any, Any) = {
        val input = Input2015.Day04

        val startsWith5Zeros = startsWithNZeros(5)
        val (n1, _) = hashStream(input).filter(p => startsWith5Zeros(p._2)).head

        val startsWith6Zeros = startsWithNZeros(6)
        val (n2, _) = hashStream(input).filter(p => startsWith6Zeros(p._2)).head

        (n1, n2)
    }
}
