package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day09 extends AoCApp {
    val preambleLength = 25
    val cipherCode = inputLines.map(_.toLong).toArray

    def findPairs[T](values: Array[T], offset: Int, length: Int): List[(T, T)] = {
        val end = offset + length

        @tailrec
        def pair(a: Int, pairs: List[(T, T)]): List[(T, T)] = {
            if (a >= end) pairs
            else {
                val newPairs = ((a + 1) until end).foldLeft(pairs) { (acc, b) =>
                    (values(a), values(b)) :: acc
                }
                pair(a + 1, newPairs)
            }
        }

        pair(offset, Nil)
    }

    def findOutlier[T](cipherCode: Array[T], preambleSize: Int, op: (T, T) => T): Option[T] = {
        @tailrec
        def find(i: Int): Option[T] = {
            if (i >= cipherCode.length) None
            else {
                val n = cipherCode(i)
                if (findPairs(cipherCode, i - preambleSize, preambleSize).exists(p => op(p._1, p._2) == n)) find(i + 1)
                else Some(n)
            }
        }

        find(preambleSize)
    }

    val outlier = findOutlier[Long](cipherCode, preambleLength, _ + _) match {
        case Some(value) => value
        case None => throw new Exception("no outlier found!")
    }

    part(1, outlier)

    def findWeakness(cipherCode: Array[Long], outlier: Long): Option[Long] = {

        @tailrec
        def findWeakSequence(windowSize: Int): Option[Seq[Long]] = {
            if (windowSize > cipherCode.length) None
            else cipherCode.sliding(windowSize).find(_.sum == outlier) match {
                case Some(value) => Some(value)
                case None => findWeakSequence(windowSize + 1)
            }
        }

        findWeakSequence(2) map { seq =>
            seq.min + seq.max
        }
    }

    val weakness = findWeakness(cipherCode, outlier) match {
        case Some(value) => value
        case None => throw new Exception("no weakness found!")
    }
    part(2, weakness)
}
