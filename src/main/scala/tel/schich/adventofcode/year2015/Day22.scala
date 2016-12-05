package tel.schich.adventofcode.year2015

import java.util.concurrent.atomic.AtomicInteger

import tel.schich.adventofcode.AoCApp
import tel.schich.adventofcode.year2015.Day21.extractNumber

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

object Day22 extends AoCApp {

    sealed trait Result {
        def terminal = false
    }

    val Debug = false
    @inline
    def println(s: => Any = ""): Unit = {
        if (Debug) Predef.println(s)
    }

    case class Success(manaUsed: Int) extends Result
    case object Defeated extends Result
    case object OutOfMana extends Result
    case class SimulationState(player: Player, enemy: Enemy, effects: Set[Spell]) extends Result
    case object CastedActiveEffect extends Result

    case class Spell(name: String, cost: Int, damage: Int = 0, heal: Int = 0, armor: Int = 0, mana: Int = 0, duration: Int = 0) {
        def decreasedDuration: Spell = copy(duration = duration - 1)
        def add(o: Spell): Spell = copy("Merge", cost + o.cost, damage + o.damage, heal + o.heal, armor + o.armor, mana + o.mana, 0)
    }

    case class Player(health: Int, armor: Int, mana: Int, manaUsed: Int = 0) {
        def applySpell(spell: Spell): Player = {
            copy(health = health + spell.heal, armor = spell.armor, mana = mana + spell.mana)
        }

        def useSpell(spell: Spell): Player = {
            val cost = spell.cost
            copy(mana = mana - cost, manaUsed = manaUsed + cost)
        }

    }

    case class Enemy(health: Int, damage: Int) {
        def applySpell(spell: Spell): Enemy = copy(health = health - spell.damage)

        def damage(player: Player): Player = {
            player.copy(health = player.health - Math.max(damage - player.armor, 1))
        }
    }

    val Seq(hpLine, damageLine) = inputLines

    val enemy = Enemy(extractNumber(hpLine), extractNumber(damageLine))

    val MagicMissile = Spell("Magic Missle", cost = 53, damage = 4)
    val Drain = Spell("Drain", cost = 73, damage = 2, heal = 2)
    val Shield = Spell("Shield", cost = 113, armor = 7, duration = 6)
    val Poison = Spell("Poison", cost = 173, damage = 3, duration = 6)
    val Recharge = Spell("Recharge", cost = 229, mana = 101, duration = 5)
    val Spells = Vector(MagicMissile, Drain, Shield, Poison, Recharge)

    def simulateRound(simulationState: SimulationState, spell: Spell, hardMode: Boolean = false): Result = {

        @inline
        def applyHardMode(player: Player) = {
            if (hardMode) player.copy(health = player.health - 1)
            else player
        }

        def wontRunOut(spell: Spell): Boolean = spell.duration > 1

        @inline
        def applyEffects(player: Player, enemy: Enemy, effects: Set[Spell]): (Player, Enemy, Set[Spell]) = {
            if (effects.isEmpty) (player, enemy, effects)
            else {
                val merge = effects.reduce(_.add(_))
                (player.applySpell(merge), enemy.applySpell(merge), effects.filter(wontRunOut).map(_.decreasedDuration))
            }
        }

        def playerTurn(player: Player, enemy: Enemy, spell: Spell, effects: Set[Spell]): Result = {
            println()
            println(s"-- Player turn --")
            println(s"- Player has ${player.health} hit points, ${player.armor} armor, ${player.mana} mana")
            println(s"- Boss has ${enemy.health} hit points")
            val (affectedPlayer, affectedEnemy, activeEffects) = applyEffects(applyHardMode(player), enemy, effects)

            if (affectedPlayer.health <= 0) Defeated
            else if (affectedEnemy.health <= 0) Success(affectedPlayer.manaUsed)
            else {
                if (spell.cost > affectedPlayer.mana) OutOfMana
                else {
                    val spellCastingPlayer = affectedPlayer.useSpell(spell)
                    println(s"Player casts ${spell.name}.")
                    if (spell.duration > 0) {
                        if (activeEffects.contains(spell)) CastedActiveEffect
                        else enemyTurn(spellCastingPlayer, affectedEnemy, activeEffects + spell)
                    } else {
                        val finalEnemy = affectedEnemy.applySpell(spell)
                        val finalPlayer = spellCastingPlayer.applySpell(spell)
                        if (finalEnemy.health <= 0) Success(finalPlayer.manaUsed)
                        else enemyTurn(finalPlayer, finalEnemy, activeEffects)
                    }
                }
            }
        }

        @inline
        def enemyTurn(player: Player, enemy: Enemy, effects: Set[Spell]): Result = {
            println()
            println(s"-- Boss turn --")
            println(s"- Player has ${player.health} hit points, ${player.armor} armor, ${player.mana} mana")
            println(s"- Boss has ${enemy.health} hit points")
            val (affectedPlayer, affectedEnemy, activeEffects) = applyEffects(player, enemy, effects)

            if (affectedEnemy.health <= 0) Success(affectedPlayer.manaUsed)
            else {
                val damagedPlayer = affectedEnemy.damage(affectedPlayer)
                if (damagedPlayer.health <= 0) Defeated
                else SimulationState(damagedPlayer, affectedEnemy, activeEffects)
            }
        }

        playerTurn(simulationState.player, simulationState.enemy, spell, simulationState.effects)
    }

    def genAndSimFights(simulationState: SimulationState, spells: ParSeq[Spell], hardMode: Boolean): ParSeq[Result] = {

        def recurse(spell: Spell) = {
            simulateRound(simulationState, spell, hardMode) match {
                case continue: SimulationState => genAndSimFights(continue, spells, hardMode)
                case result =>  List(result)
            }
        }

        spells.flatMap(recurse)
    }

    def generateAndSimulateFights(simulationState: SimulationState, spells: Seq[Spell], hardMode: Boolean): Unit = {

        val allSpells = spells.toList

        val i = new AtomicInteger(1)

        @tailrec
        def recurse(simulationState: List[SimulationState], spellStack: List[List[Spell]], results: List[Result]): List[Result] = {
            if (simulationState.isEmpty) results
            else {
                spellStack match {
                    case Nil => results
                    case Nil:: previousSpells => recurse(simulationState.tail, previousSpells, results)
                    case (spell :: otherSpells) :: previousSpells =>
                        simulateRound(simulationState.head, spell, hardMode) match {
                            case continue: SimulationState => recurse(continue :: simulationState, allSpells :: otherSpells :: previousSpells, results)
                            case result =>
                                if (i.getAndIncrement() % 10000000 == 0) Predef.println(i)
                                recurse(simulationState, otherSpells :: previousSpells, result :: results)
                        }
                }
            }
        }

        spells.par.flatMap(spell => recurse(List(simulationState), List(List(spell)), Nil))

    }

    def simulateFight(player: Player, enemy: Enemy, spells: Seq[Spell], hardMode: Boolean): Result = {
        spells.foldLeft[Result](SimulationState(player, enemy, Set.empty)) {
            case (s: SimulationState, spell) => simulateRound(s, spell, hardMode)
            case (r, _) => r
        }
    }

    val results = generateAndSimulateFights(SimulationState(Player(50, 0, 500), enemy, Set.empty), Spells, hardMode = false)
//    val results = genAndSimFights(SimulationState(Player(50, 0, 500), enemy, Set.empty), Spells.par, hardMode = false)
//    val results = generateAndSimulateFights(SimulationState(Player(10, 0, 250), Enemy(14, 8), Set.empty), Spells, hardMode = false)
//    val successfulResults = results.filter {
//        case _: Success => true
//        case _ => false
//    }
//    println(successfulResults.length)

//    val spells = Vector(
////        Poison, Recharge, Shield, Poison, MagicMissile, MagicMissile, MagicMissile, MagicMissile
////        Poison, Recharge, Shield, Poison, Recharge, Drain, Poison, MagicMissile
//        Recharge, Shield, Drain, Poison, MagicMissile
//    )
////    println(simulateFight(Player(50, 0, 500), Enemy(51, 9), spells, hardMode = true))
//    println(simulateFight(Player(10, 0, 250), Enemy(14, 8), spells, hardMode = false))

}
