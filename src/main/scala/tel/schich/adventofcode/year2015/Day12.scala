package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp

import scala.util.parsing.json._ // scala-parser-combinators artifact

sealed trait JsonValue
sealed trait JsonNumber[T] extends JsonValue {
    def value: T
}
case class JsonObject(children: Map[String, JsonValue]) extends JsonValue
case class JsonArray(children: Seq[JsonValue]) extends JsonValue
case class JsonString(string: String)
case class JsonFloat(value: Double) extends JsonNumber[Double]
case class JsonInt(value: Long) extends JsonNumber[Long]

object Day12 extends AoCApp {

    JSON.parseFull(inputText).foreach { tree =>

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
        part(1, sumOfNumbers)

        val sumOfNonRedNumbers = findNumbers(removeObjectsWith(tree, "red")).map(_.intValue).sum
        part(2, sumOfNonRedNumbers)

    }
}
