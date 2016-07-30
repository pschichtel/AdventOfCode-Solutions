import scala.annotation.tailrec

object Day19 extends AoCApp {
    println("Day 19")

    val input = inputLines.toSeq

    val subst = raw"(\w+) => (\w+)".r
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

    def allSingleReplacements(s: String, from: String, to: String): Seq[String] = {
        val parts = s.split(from, -1)

        for {i <- 1 until parts.length} yield {
            val begin = parts.slice(0, i).mkString(from)
            val end = parts.slice(i, parts.length).mkString(from)
            begin + to + end
        }
    }

    val replacements = substitutions.flatMap {
        case (from, to) => allSingleReplacements(baseString, from, to)
    }.toSet


    println(s"Part 1: ${replacements.size}")

    @tailrec
    def tokenize(s: String, tokens: Seq[String] = Nil): Seq[String] = {
        val next = s.indexWhere(Character.isUpperCase, 1)
        if (next < 0) tokens :+ s
        else tokenize(s.substring(next), tokens :+ s.substring(0, next))
    }

    // this formula is from askalski on reddit
    val tokens = tokenize(baseString)
    //                             ,
    val comma = tokens.count(_ == "Y")
    //                                  (             )
    val paran = tokens.count(t => t == "Rn" || t == "Ar")
    val stepsRequiredToReduce = tokens.length - paran - 2 * comma - 1
    println(s"Step 2: $stepsRequiredToReduce")
}
