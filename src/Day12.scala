import scala.util.parsing.json._ // scala-parser-combinators artifact

object Day12 extends AoCApp {
    println("Day 12")

    val input = inputSource.mkString.trim

    JSON.parseFull(input).foreach { tree =>

        def findNumbers(o: Any): Seq[Number] = {
            o match {
                case n: Number => Seq(n)
                case m: Map[_, Any] => m.values.flatMap(findNumbers).toSeq
                case a: Seq[Any] => a.flatMap(findNumbers)
                case _ => Nil
            }
        }

        def removeObjectsWith(o: Any, e: Any): Any = {

            o match {
                case m: Map[_, Any] =>
                    if (m.values.exists(_ == e)) Map.empty
                    else m.mapValues(v => removeObjectsWith(v, e))
                case a: List[Any] =>
                    a.map(v => removeObjectsWith(v, e))
                case e => e
            }

        }


        val sumOfNumbers = findNumbers(tree).map(_.intValue).sum
        println(s"Part 1: $sumOfNumbers")

        val sumOfNonRedNumbers = findNumbers(removeObjectsWith(tree, "red")).map(_.intValue).sum
        println(s"Part 2: $sumOfNonRedNumbers")

    }
}
