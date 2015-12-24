import scala.math._
import scala.annotation.tailrec

val input = 34000000

val N: Stream[Int] = 1 #:: N.map(_ + 1)
val doubling: Stream[Int] = 1 #:: doubling.map(_ * 2)

def numberOfPresentsFor(house: Int): Int = {
    10 * (house + N.take(house / 2).filter(house % _ == 0).sum)
}

N.map(house => (house, numberOfPresentsFor(house)))
 .takeWhile {case (house, presents) => presents < input}
 .foreach(println)

