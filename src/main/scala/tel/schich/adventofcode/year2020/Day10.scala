package tel.schich.adventofcode.year2020

import org.jblas.DoubleMatrix
import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day10 extends AoCApp {

    val values = asLines(Input2020.Day10).map(_.toInt).toList.sorted

    def countByDiff(adapters: List[Int], n: Int): Int = (0 :: adapters).zip(adapters).count(t => t._2 - t._1 == n)

    part(1, countByDiff(values, 1) * (countByDiff(values, 3) + 1))

    def countCombinations(adaptors: List[Int]): Double = {
        val outletAndAdapters = 0 :: adaptors
        val canSupply = outletAndAdapters.map { a =>
            (a, adaptors.filter { b =>
                val diff = b - a
                diff > 0 && diff <= 3
            })
        }.toMap

        val A = new DoubleMatrix(outletAndAdapters.size, outletAndAdapters.size, outletAndAdapters.flatMap { a =>
            outletAndAdapters.map { b =>
                if (canSupply(a).contains(b)) 1.0 else 0.0
            }
        }.toArray: _*)

        @tailrec
        def count(matrix: DoubleMatrix, i: Int, n: Double): Double = {
            if (i >= outletAndAdapters.size) n
            else {
                val next = matrix.mmul(A)
                count(next, i + 1, n + next.get(next.columns - 1, 0))
            }
        }

        count(A, 0, 0.0)
    }

    part(2, countCombinations(values).toLong)

}
