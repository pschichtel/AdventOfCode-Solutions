package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp


object Day17 extends AoCApp {

    val input = inputLines

    case class Container(size: Int, number: Int)

    val containers = input.zipWithIndex.map(c => Container(c._1.toInt, c._2))
    val eggnogAmount = 150

    def combinationStream[T](in: Seq[T], start: Int, until: Int): LazyList[Seq[T]] = {
        if (start <= until) in.combinations(start).to(LazyList) ++ combinationStream(in, start + 1, until)
        else LazyList.empty
    }

    val possibleCombinations = combinationStream(containers, 1, containers.length).map(_.map(_.size)).filter(_.sum == eggnogAmount)
    part(1, possibleCombinations.size)

    val shortestCombinationLength = possibleCombinations.map(_.length).min
    val shortestCombinations = possibleCombinations.filter(_.length == shortestCombinationLength)
    part(2, shortestCombinations.size)

}
