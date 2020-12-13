package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.reflect.ClassTag

object Day10 extends AoCApp {

    val values = asLines(Input2020.Day10).map(_.toInt).toList.sorted

    def countByDiff(adapters: List[Int], n: Int): Int = (0 :: adapters).zip(adapters).count(t => t._2 - t._1 == n)

    part(1, countByDiff(values, 1) * (countByDiff(values, 3) + 1))

    case class SquareMatrix[T](data: Array[T], size: Int) {
        def multiply(b: SquareMatrix[T])(implicit numeric: Numeric[T], ct: ClassTag[T]): SquareMatrix[T] = {
            val result = Array.ofDim[T](size * size)

            for {
                row <- 0 until size
                column <- 0 until size
            } {
                result(row * size + column) = (0 until size).map { i =>
                    numeric.times(data(row * size + i), b.data(i * size + column))
                }.sum
            }

            SquareMatrix(result, size)
        }

        override def toString: String = data.grouped(size).map(_.mkString(" ")).mkString("\n")
    }

    def countCombinations[T](adaptors: List[Int])(implicit numeric: Numeric[T], ct: ClassTag[T]): T = {
        val outletAndAdapters = 0 :: adaptors
        val canSupply = outletAndAdapters.map { a =>
            (a, adaptors.filter { b =>
                val diff = b - a
                diff > 0 && diff <= 3
            })
        }.toMap

        val A = SquareMatrix(outletAndAdapters.flatMap { a =>
            outletAndAdapters.map { b =>
                if (canSupply(a).contains(b)) numeric.one else numeric.zero
            }
        }.toArray, outletAndAdapters.size)

        @tailrec
        def count(matrix: SquareMatrix[T], i: Int, n: T): T = {
            if (i > adaptors.size) n
            else {
                val next = matrix.multiply(A)
                count(next, i + 1, numeric.plus(n, next.data(adaptors.size)))
            }
        }

        count(A, 1, numeric.zero)
    }

    part(2, countCombinations[Long](values))

}
