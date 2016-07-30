import scala.math._

object Day20 extends AoCApp {
    val input = 34000000

    val N: Stream[Int] = 1 #:: N.map(_ + 1)

    def numberOfPresentsFor(house: Int, factor: Int = 10, f: (Int, Int) => Boolean = (a, b) => true): Int = {
        val divisors = N.take(sqrt(house).toInt).filter(house % _ == 0).map {d =>
            val base = if (f(house, d)) d else 0
            val second = house / d
            base + (if (f(house, second)) second else 0)
        }
        factor * divisors.sum
    }

    val (house, _) = N.take(input / 10).map(house => (house, numberOfPresentsFor(house)))
        .filter {case (_, presents) => presents >= input}
        .head

    println(s"Day 1: $house")


    val (houseLimited, _) = N.take(input / 10).map(house => (house, numberOfPresentsFor(house, 11, _ / _ < 50)))
        .filter {case (_, presents) => presents >= input}
        .head

    println(s"Day 2: $houseLimited")
}