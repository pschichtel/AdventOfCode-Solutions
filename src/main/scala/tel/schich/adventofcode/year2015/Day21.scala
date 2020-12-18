package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day21 extends AoCApp {

    def extractNumber(s: String): Int = {
        val num = "[^\\d]*(\\d+).*".r
        val num(n) = s
        n.toInt
    }

    //          (Name, Cost, Damage, Armor)
    type Item = (String, Int, Int, Int)

    //            (HP, Damage, Armor)
    type Entity = (Int, Int, Int)

    //                    (Cost, Damage, Armor)
    type ItemAttributes = (Int, Int, Int)
    private val ZeroEntity = (0, 0, 0)

    def sumItems(items: Seq[Item]): ItemAttributes = {
        items.foldLeft(ZeroEntity) {
            case ((allHP, allDamage, allArmor), (_, newHP, newDamage, newArmor)) =>
                (allHP + newHP, allDamage + newDamage, allArmor + newArmor)
        }
    }

    def buyItems(requirements: Seq[(Range, Seq[Item])]): Seq[ItemAttributes] = {

        val combinations = requirements.map {
            case (range, items) => range.flatMap(items.combinations)
        }

        def join(left: Seq[Seq[Item]], right: Seq[Seq[Item]]): Seq[Seq[Item]] = {
            for {
                l <- left
                r <- right
            } yield l ++ r
        }

        val joined = combinations.reduce(join)

        joined.map(sumItems)
    }

    def fightSimulation(attacker: Entity, defender: Entity): Boolean = {
        val (_, damage, _) = attacker
        val (hitPoints, defenderDamage, armor) = defender

        val actualDamage = math.max(damage - armor, 1)
        val newHitPoints = hitPoints - actualDamage

        if (newHitPoints > 0) !fightSimulation((newHitPoints, defenderDamage, armor), attacker)
        else true
    }

    private val weapons = Seq(
        ("Dagger",      8, 4, 0),
        ("Shortsword", 10, 5, 0),
        ("Warhammer",  25, 6, 0),
        ("Longsword",  40, 7, 0),
        ("Greataxe",   74, 8, 0)
    )

    private val armors = Seq(
        ("Leather",     13, 0, 1),
        ("Chainmail",   31, 0, 2),
        ("Splintmail",  53, 0, 3),
        ("Bandedmail",  75, 0, 4),
        ("Platemail",  102, 0, 5)
    )

    private val rings = Seq(
        ("Damage +1",   25, 1, 0),
        ("Damage +2",   50, 2, 0),
        ("Damage +3",  100, 3, 0),
        ("Defense +1",  20, 0, 1),
        ("Defense +2",  40, 0, 2),
        ("Defense +3",  80, 0, 3)
    )


    override def solution: (Any, Any) = {

        val List(hitPointsLine, damageLine, armorLine) = asLines(Input2015.Day21).toList

        val enemy = (extractNumber(hitPointsLine), extractNumber(damageLine), extractNumber(armorLine))

        val itemBuyRequirements = Seq((1 to 1, weapons), (0 to 1, armors), (0 to 2, rings))

        val possiblePlayersWithCost = buyItems(itemBuyRequirements).map { attributes =>
            val (cost, damage, armor) = attributes

            (cost, (100, damage, armor))
        }

        val simulatedFights = possiblePlayersWithCost.map {
            case (cost, player) => (cost, fightSimulation(player, enemy), player)
        }

        val (minimumCost, _, _) = simulatedFights.filter(_._2).minBy(_._1)
        val (maximumCost, _, _) = simulatedFights.filter(!_._2).maxBy(_._1)

        (minimumCost, maximumCost)
    }
}
