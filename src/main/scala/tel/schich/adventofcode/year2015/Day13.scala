package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.generated.Input2015
import tel.schich.adventofcode.shared.AoCApp

object Day13 extends AoCApp {

    val input = asLines(Input2015.Day13)

    type Relation = (String, String, Int)

    val relation = "(\\w+) would (lose|gain) (\\d+) happiness units by sitting next to (\\w+).".r
    val relations: Set[Relation] = input.map {
        case relation(s, "lose", h, o) => (s, o, -h.toInt)
        case relation(s, "gain", h, o) => (s, o, h.toInt)
    }.toSet


    def peopleFrom(relations: Set[Relation]): Set[String] = relations.map(_._1)

    def optimalSeating(relations: Set[Relation]): (Seq[String], Int) = {

        val happynessByPair = relations.map(r => (r._1, r._2) -> r._3).toMap

        val people = peopleFrom(relations)

        val plans = people.toSeq.permutations.map { plan =>
            val pairs = plan.sliding(2).toSeq :+ Seq(plan.head, plan.last)
            val change = pairs.map(_.toList).map {
                case l :: r :: _ => happynessByPair((l, r)) + happynessByPair((r, l))
                case _ => 0
            }.sum
            (plan, change)
        }

        plans.maxBy(_._2)
    }

    val (_, maxWithoutMe) = optimalSeating(relations)
    part(1, maxWithoutMe)

    val relationsWithMe = relations ++ peopleFrom(relations).flatMap(p => Set((p, "me", 0), ("me", p, 0)))
    val (_, maxWithMe) = optimalSeating(relationsWithMe)
    part(2, maxWithMe)
}
