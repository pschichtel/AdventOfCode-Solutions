package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

object Day09 extends AoCApp {

    def findPairs[T](values: ArraySeq[T], offset: Int, length: Int): List[(T, T)] = {
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

    def findOutlier[T](cipherCode: ArraySeq[T], preambleSize: Int)(implicit numeric: Numeric[T]): Option[T] = {
        @tailrec
        def find(i: Int): Option[T] = {
            if (i >= cipherCode.length) None
            else {
                val n = cipherCode(i)
                if (findPairs(cipherCode, i - preambleSize, preambleSize).exists(p => numeric.plus(p._1, p._2) == n)) find(i + 1)
                else Some(n)
            }
        }

        find(preambleSize)
    }

    def findWeakness[T](cipherCode: ArraySeq[T], outlier: T)(implicit numeric: Numeric[T]): Option[T] = {
        @tailrec
        def findWeakSequence(windowSize: Int): Option[Seq[T]] = {
            if (windowSize > cipherCode.length) None
            else cipherCode.sliding(windowSize).find(_.sum == outlier) match {
                case Some(value) => Some(value)
                case None => findWeakSequence(windowSize + 1)
            }
        }

        findWeakSequence(2) map { seq =>
            numeric.plus(seq.min, seq.max)
        }
    }

    override def solution: (Any, Any) = {
        val preambleLength = 25
        val cipherCode = asLines(Input2020.Day09).map(_.toLong)

        val Some(outlier) = findOutlier(cipherCode, preambleLength)
        val Some(weakness) = findWeakness(cipherCode, outlier)

        (outlier, weakness)
    }
}
