package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day21.extractNumber

import scala.annotation.tailrec

object Day22 extends AoCApp {

    sealed trait Result {
        def terminal = false
    }

    case class Success(manaUsed: Int) extends Result
    case object Defeated extends Result
    case object OutOfMana extends Result
    case object OutOfSpells extends Result
    case object CastedActiveEffect extends Result

    case class Spell(name: String, cost: Int, damage: Int = 0, heal: Int = 0, shield: Int = 0, mana: Int = 0, duration: Int = 0) {
        def decreasedDuration: Spell = this.copy(duration = duration - 1)
    }

    case class Player(health: Int, armor: Int, mana: Int, spells: Seq[Spell] = Nil, manaUsed: Int = 0) {
        def applySpell(spell: Spell): Player = {
            val newArmor =
                if (spell.shield == 0) armor
                else spell.shield
            copy(health = health + spell.heal, armor = newArmor, mana = mana + spell.mana)
        }

        def useSpell(): (Player, Spell) = {
            val cost = spells.head.cost
            (copy(spells = spells.tail, mana = mana - cost, manaUsed = manaUsed + cost), spells.head)
        }

    }

    case class Enemy(health: Int, damage: Int) {
        def applySpell(spell: Spell): Enemy = copy(health = health - spell.damage)

        def damage(player: Player): Player = {
            val dmg = Math.max(damage - player.armor, 1)
//            if (player.armor > 0) println(s"Boss attacks for $damage - ${player.armor} = $dmg damage!")
//            else println(s"Boss attacks for $damage damage!")
            player.copy(health = player.health - dmg)
        }
    }

    val Seq(hpLine, damageLine) = inputLines

    val enemy = Enemy(extractNumber(hpLine), extractNumber(damageLine))

    val MagicMissile = Spell("Magic Missle", cost = 53, damage = 4)
    val Drain = Spell("Drain", cost = 73, damage = 2, heal = 2)
    val Shield = Spell("Shield", cost = 113, shield = 7, duration = 6)
    val Poison = Spell("Poison", cost = 173, damage = 3, duration = 6)
    val Recharge = Spell("Recharge", cost = 229, mana = 101, duration = 5)
    val Spells = Seq(MagicMissile, Drain, Shield, Poison, Recharge)

    def simulateFight(player: Player, enemy: Enemy): Result = {

        @inline
        def applyEffects(player: Player, enemy: Enemy, effects: Set[Spell]): (Player, Enemy, Set[Spell]) = {
            effects.foldLeft((player, enemy, Set.empty[Spell])) {
                case ((p, e, finalSpells), spell) =>
                    val appliedSpell = spell.decreasedDuration
//                    println(s"${appliedSpell.name} does its thing; tis timer is now ${appliedSpell.duration}.")
                    val newFinalSpells =
                        if (appliedSpell.duration == 0) {
//                            println(s"${appliedSpell.name} wears off.")
                            finalSpells
                        }
                        else finalSpells + appliedSpell

                    (p.applySpell(spell), e.applySpell(spell), newFinalSpells)
            }
        }

        @tailrec
        def turn(player: Player, enemy: Enemy, playerTurn: Boolean, effects: Set[Spell]): Result = {
//            println()
//            println(s"-- ${if (playerTurn) "Player" else "Boss"} turn --")
//            println(s"- Player has ${player.health} hit points, ${player.armor} armor, ${player.mana} mana")
//            println(s"- Boss has ${enemy.health} hit points")
            val (affectedPlayer, affectedEnemy, activeEffects) = applyEffects(player, enemy, effects)

            if (playerTurn) {
                if (affectedEnemy.health <= 0) Success(affectedPlayer.manaUsed)
                else if (affectedPlayer.spells.isEmpty) OutOfSpells
                else if (affectedPlayer.mana < affectedPlayer.spells.head.cost) OutOfMana
                else {
                    val (spellCastingPlayer, spell) = affectedPlayer.useSpell()
//                    println(s"Player casts ${spell.name}.")
                    if (spell.duration > 0) {
                        if (activeEffects.contains(spell)) CastedActiveEffect
                        else turn(spellCastingPlayer, affectedEnemy, playerTurn = false, activeEffects + spell)
                    } else {
                        val finalEnemy = affectedEnemy.applySpell(spell)
                        val finalPlayer = spellCastingPlayer.applySpell(spell)
                        if (finalEnemy.health <= 0) Success(finalPlayer.manaUsed)
                        else turn(finalPlayer, finalEnemy, playerTurn = false, activeEffects)
                    }
                }
            } else {
                if (affectedEnemy.health <= 0) Success(affectedPlayer.manaUsed)
                else {
                    val damagedPlayer = affectedEnemy.damage(affectedPlayer)
                    if (damagedPlayer.health <= 0) Defeated
                    else turn(damagedPlayer, affectedEnemy, playerTurn = true, activeEffects)
                }
            }
        }

        turn(player, enemy, playerTurn = true, Set.empty)
    }

    var minMana = Integer.MAX_VALUE
    var lastMana = 0
    var avgLen: Double = 0
    var countedLen: Double = 0

    private def generateAndSimulateFights(playerBase: Player, enemy: Enemy, currentSeq: Seq[Spell], spells: Seq[Spell]): Seq[Result] = {
        val result = simulateFight(playerBase.copy(spells = currentSeq), enemy)

        result match {
            case Success(mana) =>
                if (mana < minMana) minMana = mana
                lastMana = mana
            case _ =>
        }
        avgLen = (avgLen * countedLen + currentSeq.length) / (countedLen + 1)
        countedLen += 1
        println(s"${currentSeq.length} $minMana $lastMana $avgLen $result")
//        println(s"Spells: $result $currentSeq")

        result match {
            case OutOfSpells =>
                spells.flatMap { spell =>
                    generateAndSimulateFights(playerBase, enemy, currentSeq :+ spell, spells)
                }
            case _ => Seq(result)
        }
    }

    val results = generateAndSimulateFights(Player(50, 0, 500), enemy, Vector.empty, Spells)
//
    val successfulResults = results.filter {
        case _: Success => true
        case _ => false
    }
    println(successfulResults.length)

//    val spells = Vector(Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Recharge", 229,0,0,0,101,5), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Shield", 113,0,0,7,0,6), Spell("Recharge", 229,0,0,0,101,5), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Magic Missile", 53,4,0,0,0,0), Spell("Recharge", 229,0,0,0,101,5), Spell("Recharge", 229,0,0,0,101,5), Spell("Recharge", 229,0,0,0,101,5), Spell("Drain", 73,2,2,0,0,0), Spell("Recharge", 229,0,0,0,101,5), Spell("Shield", 113,0,0,7,0,6), Spell("Recharge", 229,0,0,0,101,5), Spell("Recharge", 229,0,0,0,101,5), Spell("Shield", 113,0,0,7,0,6), Spell("Shield", 113,0,0,7,0,6), Spell("Recharge", 229,0,0,0,101,5), Spell("Drain", 73,2,2,0,0,0), Spell("Poison", 173,3,0,0,0,6))
//    simulateFight(Player(50, 0, 500, spells), enemy)
//    val spells = Vector(
//        Recharge, Shield, Drain, Poison, MagicMissile
//    )
//    println(simulateFight(Player(10, 0, 250, spells), Enemy(14, 8)))

}
