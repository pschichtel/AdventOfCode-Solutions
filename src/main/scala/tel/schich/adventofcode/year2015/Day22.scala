package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day21.extractNumber

object Day22 extends AoCApp {

    val List(hpLine, damageLine) = inputLines

    val Player = (500, 0, 500)
    val Enemy = (extractNumber(hpLine), 0, extractNumber(damageLine))

    case class Spell(cost: Int, damage: Int = 0, heal: Int = 0, shield: Int = 0, mana: Int = 0, duration: Int = 0) {

    }

    val MagicMissile = Spell(cost = 53, damage = 4)
    val Drain        = Spell(cost = 73, damage = 2, heal = 2)
    val Shield       = Spell(cost = 113, shield = 7, duration = 6)
    val Poison       = Spell(cost = 173, damage = 3, duration = 6)
    val Recharge     = Spell(cost = 229, mana = 101, duration = 5)

    val Spells = Seq(MagicMissile, Drain, Shield, Poison, Recharge)

}
