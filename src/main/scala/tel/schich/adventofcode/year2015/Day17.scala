package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp


object Day17 extends AoCApp {
    case class Container(size: Int, number: Int)

    def combinationStream[T](in: Seq[T], start: Int, until: Int): LazyList[Seq[T]] = {
        if (start <= until) in.combinations(start).to(LazyList) ++ combinationStream(in, start + 1, until)
        else LazyList.empty
    }

    override def solution: (Any, Any) = {
        val input = asLines(Input2015.Day17)

        val containers = input.zipWithIndex.map(c => Container(c._1.toInt, c._2))
        val eggnogAmount = 150

        val possibleCombinations = combinationStream(containers, 1, containers.length).map(_.map(_.size)).filter(_.sum == eggnogAmount)

        val shortestCombinationLength = possibleCombinations.map(_.length).min
        val shortestCombinations = possibleCombinations.filter(_.length == shortestCombinationLength)

        (possibleCombinations.size, shortestCombinations.size)
    }
}
