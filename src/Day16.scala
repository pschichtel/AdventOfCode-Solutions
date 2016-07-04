
object Day16 extends AoCApp {
    println("Day 16")

    val input = sourceFromCP("day16.txt").mkString.trim.split('\n').map(_.trim).toSeq

    val entry = raw"Sue (\d+): (.+)".r
    val property = raw"(\w+): (\d+)".r

    type Sue = (Int, Map[String, Int])

    val sues = input.map {
        case entry(n, properties) => (n.toInt, property.findAllIn(properties).map { case property(n, v) => (n, v.toInt) }.toMap)
    }

    def suesWith(sues: Seq[Sue], name: String, value: Int, cmp: (Int, Int) => Boolean) = {
        sues.filter {
            case (_, p) if p.contains(name) => cmp(p(name), value)
            case _ => true
        }
    }


    def eq(a: Int, b: Int) = a == b

    def gt(a: Int, b: Int) = a > b

    def lt(a: Int, b: Int) = a < b

    val MFCSAM = Map[String, (Int, (Int, Int) => Boolean)](
        ("children", (3, eq _)),
        ("cats", (7, gt _)),
        ("samoyeds", (2, eq _)),
        ("pomeranians", (3, lt _)),
        ("akitas", (0, eq _)),
        ("vizslas", (0, eq _)),
        ("goldfish", (5, lt _)),
        ("trees", (3, gt _)),
        ("cars", (2, eq _)),
        ("perfumes", (1, eq _))
    )


    val possibleSues = MFCSAM.foldLeft(sues) {
        case (s, (n, (v, _))) => suesWith(s, n, v, eq)
    }

    println("Part 1: " + (possibleSues.headOption match {
        case Some((n, _)) => n + ""
        case None => "n/a"
    }))

    val correctPossibleSues = MFCSAM.foldLeft(sues) {
        case (s, (n, (v, c))) => suesWith(s, n, v, c)
    }

    println("Part 2: " + (correctPossibleSues.headOption match {
        case Some((n, _)) => n + ""
        case None => "n/a"
    }))
}