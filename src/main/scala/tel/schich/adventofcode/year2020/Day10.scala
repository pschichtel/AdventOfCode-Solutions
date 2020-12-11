package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Day10 extends AoCApp {

    val values = asLines(Input2020.Day10).map(_.toInt).toList.sorted

    def countByDiff(adapters: List[Int], n: Int): Int = (0 :: adapters).zip(adapters).count(t => t._2 - t._1 == n)

    part(1, countByDiff(values, 1) * (countByDiff(values, 3) + 1))

    type Matrix[T] = Array[Array[T]]

    def multiply[T](A: Matrix[T], B: Matrix[T])(implicit numeric: Numeric[T], ct: ClassTag[T]): Matrix[T] = {
        val size = A.length
        val result = Array.ofDim[T](size, size)

        for (column <- 0 until size; row <- 0 until size) {
            result(column)(row) = (0 until size).map { i =>
                numeric.times(A(i)(row), B(column)(i))
            }.sum
        }

        result
    }

    def printMatrix(matrix: Matrix[_]): Unit = {
        println(matrix.map(_.mkString(" ")).mkString("\n"))
    }

    def countCombinations[T](adaptors: List[Int])(implicit numeric: Numeric[T], ct: ClassTag[T]): T = {
        val canSupply = (0 :: values).map { a =>
            (a, values.filter { b =>
                val diff = b - a
                diff > 0 && diff <= 3
            })
        }.toMap

        val A: Matrix[T] = (0 :: values).map { a =>
            (0 :: values).map { b =>
                if (canSupply(a).contains(b)) numeric.one else numeric.zero
            }.toArray
        }.toArray

        @tailrec
        def count(matrix: Matrix[T], i: Int, n: T): T = {
            if (i > adaptors.size) n
            else {
                val next = multiply(matrix, A)
                count(next, i + 1, numeric.plus(n, next(0)(adaptors.size)))
            }
        }

        count(A, 1, numeric.zero)
    }

    part(2, countCombinations[Long](values))

}
