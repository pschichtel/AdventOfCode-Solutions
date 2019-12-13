package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp

object Day16 extends AoCApp {

    val input = inputLines

    val entry = "Sue (\\d+): (.+)".r
    val property = "(\\w+): (\\d+)".r

    type Sue = (Int, Map[String, Int])

    val sues = input.map {
        case entry(n, properties) => (n.toInt, property.findAllIn(properties).map { case property(n, v) => (n, v.toInt) }.toMap)
    }

    def suesWith(sues: Seq[Sue], name: String, value: Int, cmp: (Int, Int) => Boolean): Seq[(Int, Map[String, Int])] = {
        sues.filter {
            case (_, p) if p.contains(name) => cmp(p(name), value)
            case _ => true
        }
    }


    def eq(a: Int, b: Int): Boolean = a == b

    def gt(a: Int, b: Int): Boolean = a > b

    def lt(a: Int, b: Int): Boolean = a < b

    val MFCSAM = Map[String, (Int, (Int, Int) => Boolean)](
        ("children", (3, eq)),
        ("cats", (7, gt)),
        ("samoyeds", (2, eq)),
        ("pomeranians", (3, lt)),
        ("akitas", (0, eq)),
        ("vizslas", (0, eq)),
        ("goldfish", (5, lt)),
        ("trees", (3, gt)),
        ("cars", (2, eq)),
        ("perfumes", (1, eq))
    )


    val possibleSues = MFCSAM.foldLeft(sues) {
        case (s, (n, (v, _))) => suesWith(s, n, v, eq)
    }

    part(1, possibleSues.headOption match {
        case Some((n, _)) => s"$n"
        case None => "n/a"
    })

    val correctPossibleSues = MFCSAM.foldLeft(sues) {
        case (s, (n, (v, c))) => suesWith(s, n, v, c)
    }

    part(2, correctPossibleSues.headOption match {
        case Some((n, _)) => s"$n"
        case None => "n/a"
    })
}
