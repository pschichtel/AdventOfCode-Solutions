package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day19 extends AoCApp {

    def allSingleReplacements(s: String, from: String, to: String): Seq[String] = {
        val parts = s.split(from, -1)

        for {i <- 1 until parts.length} yield {
            val begin = parts.slice(0, i).mkString(from)
            val end = parts.slice(i, parts.length).mkString(from)
            begin + to + end
        }
    }



    @tailrec
    def tokenize(s: String, tokens: Seq[String] = Nil): Seq[String] = {
        val next = s.indexWhere(Character.isUpperCase, 1)
        if (next < 0) tokens :+ s
        else tokenize(s.substring(next), tokens :+ s.substring(0, next))
    }

    override def solution: (Any, Any) = {

        val input = asLines(Input2015.Day19)

        val subst = "(\\w+) => (\\w+)".r
        val substitutions = input.collect {
            case subst(from, to) => (from, to)
        }
        val baseString = input.last

        /*
        val substitutions = Seq(
            ("H" -> "HO"),
            ("H" -> "OH"),
            ("O" -> "HH")
        )
        val baseString = "HOH"
        */

        val replacements = substitutions.flatMap {
            case (from, to) => allSingleReplacements(baseString, from, to)
        }.toSet

        // this formula is from askalski on reddit
        val tokens = tokenize(baseString)
        //                             ,
        val comma = tokens.count(_ == "Y")
        //                                  (             )
        val paran = tokens.count(t => t == "Rn" || t == "Ar")
        val stepsRequiredToReduce = tokens.length - paran - 2 * comma - 1

        (replacements.size, stepsRequiredToReduce)
    }
}
