package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day14 extends AoCApp {

    private val RaceDuration = 2503

    type Deer = (String, Int, Int, Int)
    type DeerState = (Deer, Boolean, Int, Int, Int)

    def simulate(deers: Set[Deer], duration: Int = RaceDuration): Seq[DeerState] = {

        val startStates = deers.toSeq.map(d => (d, false, 0, 0, 0))

        (1 to duration).foldLeft(startStates) { (states, _) =>

            val nextStates = states.map {
                case ((n, v, t, r), s, ts, d, p) =>
                    val newTs = if (s && ts == r) 1
                    else if (s && ts < r) ts + 1
                    else if (!s && ts == t) 1
                    else if (!s && ts < t) ts + 1
                    else ts
                    val newS = if (s && ts == r || !s && ts == t) !s else s

                    val newD = if (!newS) d + v else d

                    ((n, v, t, r), newS, newTs, newD, p)
            }

            val sortedStates = nextStates.sortBy(-_._4)
            val leaders = sortedStates.span(_._4 == sortedStates.head._4)._1.map(_._1._1).toSet

            nextStates.map {
                case sd if leaders.contains(sd._1._1) => sd.copy(_5 = sd._5 + 1)
                case sd => sd
            }

        }

    }

    override def solution: (Any, Any) = {

        val input = asLines(Input2015.Day14)
        val deer = "(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds.".r
        val deers = input.map {
            case deer(n, v, t, r) => (n, v.toInt, t.toInt, r.toInt)
        }.toSet

        val (_, _, _, distance, _) = simulate(deers).maxBy(_._4)
        val (_, _, _, _, score) = simulate(deers).maxBy(_._5)

        (distance, score)
    }
}
