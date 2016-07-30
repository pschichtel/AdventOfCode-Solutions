
object Day17 extends AoCApp {
    println("Day 17")

    val input = inputLines.toSeq

    case class Container(size: Int, number: Int)

    val containers = input.zipWithIndex.map(c => Container(c._1.toInt, c._2))
    val eggnogAmount = 150

    def combinationStream[T](in: Seq[T], start: Int, until: Int): Stream[Seq[T]] = {
        if (start <= until) in.combinations(start).toStream ++ combinationStream(in, start + 1, until)
        else Stream.empty
    }

    val possibleCombinations = combinationStream(containers, 1, containers.length).map(_.map(_.size)).filter(_.sum == eggnogAmount)
    println(s"Part 1: ${possibleCombinations.size}")

    val shortestCombinationLength = possibleCombinations.map(_.length).min
    val shortestCombinations = possibleCombinations.filter(_.length == shortestCombinationLength)
    println(s"Part 2: ${shortestCombinations.size}")

}
