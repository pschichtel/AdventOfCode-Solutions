import scala.io.Source
import scala.math._

println("Day 17")

val input = Source.fromFile("day17.txt").mkString.trim.split('\n').map(_.trim).toSeq

case class Container(size: Int, number: Int)
val containers = input.zipWithIndex.map(c => Container(c._1.toInt, c._2))
val eggnogAmount = 150

def combinationStream[T](in: Seq[T], start: Int, until: Int): Stream[Seq[T]] = {
    if (start <= until) in.combinations(start).toStream ++ combinationStream(in, start + 1, until)
    else Stream.empty
}

val containers_ = Seq(Container(20, 0), Container(15, 1), Container(10, 2), Container(5, 3), Container(5, 4))
val eggnogAmount_ = 25

val possibleCombinations = combinationStream(containers, 1, containers.length).map(_.map(_.size)).filter(_.sum == eggnogAmount)
println(s"Part 1: ${possibleCombinations.size}")

val shortestCombinationLength = possibleCombinations.map(_.length).min
val shortestCombinations = possibleCombinations.filter(_.length == shortestCombinationLength)
println(s"Part 2: ${shortestCombinations.size}")

