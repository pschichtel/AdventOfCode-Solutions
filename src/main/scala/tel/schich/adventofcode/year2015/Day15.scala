package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.math._

object Day15 extends AoCApp {

    type Taste = (Int, Int, Int, Int, Int)
    type Ingredient = (String, Int => Taste)
    private val ZeroTaste = (0, 0, 0, 0, 0)
    private val UnitTaste = (1, 1, 1, 1, 1)

    def combine(t1: Taste, t2: Taste)(p: (Int, Int) => Int): Taste = {
        (p(t1._1, t2._1),
            p(t1._2, t2._2),
            p(t1._3, t2._3),
            p(t1._4, t2._4),
            p(t1._5, t2._5))
    }

    def sum(tastes: Iterable[Taste]): Taste = {
        tastes.foldLeft(ZeroTaste)((acc, tas) => combine(acc, tas)(_ + _))
    }

    def scoreTaste(t: Taste): Int = {
        //-----v ignore calories
        t.copy(_5 = 1).productIterator.map(_.asInstanceOf[Int]).foldLeft(1)(max(0, _) * max(0, _))
    }

    def ingredientDistributions(n: Int, r: Int = 100): LazyList[Seq[Int]] = {
        val range = List.range(0, r + 1)

        (1 until n).foldLeft(range.to(LazyList).map(List(_))) { (s, _) =>
            s.flatMap(r => range.to(LazyList).map(_ :: r)).filter(_.sum <= r)
        }.filter(_.sum == 100)
    }

    def mixtures(ingredients: Seq[Ingredient]): LazyList[(Int, Int, Int, Int, Int)] = {
        ingredientDistributions(ingredients.length).map { dist =>
            val plan = ingredients.zip(dist)
            sum(plan.map {
                case ((_, t), amount) => t(amount)
            })
        }
    }

    override def solution: (Any, Any) = {

        val input = asLines(Input2015.Day15)

        val ingredient = "(\\w+): capacity (-?\\d+), durability (-?\\d+), flavor (-?\\d+), texture (-?\\d+), calories (-?\\d+)".r

        val ingredients = input.map {
            case ingredient(n, cp, d, f, t, cl) =>
                val cap = cp.toInt
                val dur = d.toInt
                val fla = f.toInt
                val tex = t.toInt
                val cal = cl.toInt
                (n, (n: Int) => (cap * n, dur * n, fla * n, tex * n, cal * n))
        }

        val bestScore = mixtures(ingredients).map(scoreTaste).max
        val bestScoreWithNCalories = mixtures(ingredients).filter(_._5 == 500).map(scoreTaste).max

        (bestScore, bestScoreWithNCalories)
    }
}
