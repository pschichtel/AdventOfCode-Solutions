package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.year2015.Day21.extractNumber

import scala.annotation.tailrec

object Day22 extends AoCApp {

    sealed trait Result
    case class Success(state: SimulationState) extends Result
    case object CastedActiveEffect extends Result
    case object Defeated extends Result
    case object OutOfMana extends Result
    case class SimulationState(playerHealth: Int, playerArmor: Int, playerMana: Int, enemyHealth: Int,
                               enemyDamage: Int, manaUsed: Int = 0, effects: Vector[Spell] = Vector.empty,
                               usedSpells: Vector[Spell] = Vector.empty) extends Result {

        def applySpell(spell: Spell): Result = {
            val cost = spell.cost
            val nowUsedSpells = usedSpells :+ spell
            if (cost > playerMana) OutOfMana
            else {
                if (spell.duration > 0) {
                    if (effects.exists(_.name == spell.name)) CastedActiveEffect
                    else copy(playerMana = playerMana - cost, manaUsed = manaUsed + cost, effects = effects :+ spell, usedSpells = nowUsedSpells)
                } else {
                    copy(playerHealth + spell.heal, playerArmor, playerMana - cost, enemyHealth - spell.damage, enemyDamage, manaUsed + cost, effects, nowUsedSpells)
                }
            }
        }

        def damagePlayer(): SimulationState = {
            copy(playerHealth = playerHealth - Math.max(enemyDamage - playerArmor, 1))
        }

        def applyHardMode(applyHardMode: Boolean): SimulationState = {
            if (applyHardMode) copy(playerHealth - 1)
            else this
        }

        def applyEffects(): SimulationState = {
            def wontRunOut(spell: Spell): Boolean = spell.duration > 1

            if (effects.isEmpty) {
                // player armor can only be set via shield spell, so without spells the armor must be reset to 0
                if (playerArmor > 0) copy(playerArmor = 0)
                else this
            }
            else {
                val merge = effects.reduce(_.add(_))

                copy(playerHealth + merge.heal, merge.armor, playerMana + merge.mana, enemyHealth - merge.damage, enemyDamage, manaUsed, effects.filter(wontRunOut).map(_.decreasedDuration))
            }
        }
    }

    case class Spell(name: String, cost: Int, damage: Int = 0, heal: Int = 0, armor: Int = 0, mana: Int = 0, duration: Int = 0) {
        def decreasedDuration: Spell = copy(duration = duration - 1)
        def add(o: Spell): Spell = copy("Merge", cost + o.cost, damage + o.damage, heal + o.heal, armor + o.armor, mana + o.mana, 0)
    }


    private val MagicMissile = Spell("Magic Missle", cost = 53, damage = 4)
    private val Drain = Spell("Drain", cost = 73, damage = 2, heal = 2)
    private val Shield = Spell("Shield", cost = 113, armor = 7, duration = 6)
    private val Poison = Spell("Poison", cost = 173, damage = 3, duration = 6)
    private val Recharge = Spell("Recharge", cost = 229, mana = 101, duration = 5)
    private val Spells = Vector(MagicMissile, Drain, Shield, Poison, Recharge)

    def simulateRound(simulationState: SimulationState, spell: Spell, hardMode: Boolean): Result = {

        def playerTurn(state: SimulationState, spell: Spell): Result = {
            val hardModeState = state.applyHardMode(hardMode)
            if (hardModeState.playerHealth <= 0) Defeated
            else {
                val affectedState = hardModeState.applyEffects()

                if (affectedState.playerHealth <= 0) Defeated
                else if (affectedState.enemyHealth <= 0) Success(affectedState)
                else {
                    affectedState.applySpell(spell) match {
                        case spellCastedState: SimulationState =>
                            if (spellCastedState.enemyHealth <= 0) Success(spellCastedState)
                            else enemyTurn(spellCastedState)
                        case result => result

                    }
                }
            }
        }

        @inline
        def enemyTurn(state: SimulationState): Result = {
            val affectedState = state.applyEffects()

            if (affectedState.enemyHealth <= 0) Success(affectedState)
            else {
                val damagedState = affectedState.damagePlayer()
                if (damagedState.playerHealth <= 0) Defeated
                else damagedState
            }
        }

        playerTurn(simulationState, spell)
    }

    def simulateFight(state: SimulationState, spells: Seq[Spell], hardMode: Boolean): Result = {
        spells.foldLeft[Result](state) {
            case (s: SimulationState, spell) => simulateRound(s, spell, hardMode)
            case (r, _) => r
        }
    }

    def findCheapestFight(startState: SimulationState, availableSpells: List[Spell], hardMode: Boolean = false): Option[SimulationState] = {

        @tailrec
        def recurse(stateStack: List[SimulationState], spellStack: List[List[Spell]], currentBest: Option[SimulationState]): Option[SimulationState] = {
            if (stateStack.isEmpty) currentBest
            else {
                spellStack match {
                    case Nil => currentBest
                    case Nil :: previousSpells => recurse(stateStack.tail, previousSpells, currentBest)
                    case (spell :: otherSpells) :: previousSpells =>
                        simulateRound(stateStack.head, spell, hardMode) match {
                            case continue: SimulationState =>
                                currentBest match {
                                    case Some(bestState) if continue.manaUsed >= bestState.manaUsed =>
                                        recurse(stateStack.tail, previousSpells, currentBest)
                                    case _ =>
                                        recurse(continue :: stateStack, availableSpells :: otherSpells :: previousSpells, currentBest)
                                }
                            case Success(finalState) =>
                                val newBest = currentBest match {
                                    case Some(bestState) =>
                                        if (finalState.manaUsed < bestState.manaUsed) Some(finalState)
                                        else currentBest
                                    case _ => Some(finalState)
                                }
                                recurse(stateStack, otherSpells :: previousSpells, newBest)
                            case _ =>
                                recurse(stateStack, otherSpells :: previousSpells, currentBest)
                        }
                }
            }
        }

        recurse(List(startState), List(availableSpells), None)

    }

    override def solution: (Any, Any) = {
        val Seq(hpLine, damageLine) = asLines(Input2015.Day22)
        val startState = SimulationState(50, 0, 500, extractNumber(hpLine), extractNumber(damageLine))

        val Some(cheapestEasyFight) = findCheapestFight(startState, Spells.toList)
        val Some(cheapestHardFight) = findCheapestFight(startState, Spells.toList, hardMode = true)

        (cheapestEasyFight.manaUsed, cheapestHardFight.manaUsed)
    }
}
