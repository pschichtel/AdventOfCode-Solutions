package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day11 extends AoCApp {

    private val generator = "(\\w+) generator".r
    private val microchip = "([a-zA-Z]+)-compatible microchip".r

    sealed trait TechObject {
        def material: String
    }
    case class Generator(material: String) extends TechObject
    case class Microchip(material: String) extends TechObject

    override def solution: (Any, Any) = {
        val floors: Seq[Seq[TechObject]] = asLines(Input2016.Day11).map { s =>
            generator.findAllMatchIn(s).map(m => Generator(m.group(1))).toList ++
                microchip.findAllMatchIn(s).map(m => Microchip(m.group(1)))
        }
        val startFloor = 0

        floors.foreach(println)

        (Unsolved, Unsolved)
    }
}
