package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec

object Day10 extends AoCApp {

    val valueToBot = "value (\\d+) goes to (bot|output) (\\d+)".r
    val botGivesLowAndHigh = "bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)".r

    sealed trait Instruction
    sealed trait Source
    sealed trait Target

    case class Value(value: Int) extends Source
    case class Bot(id: Int) extends Source with Target
    case class Output(id: Int) extends Target

    case class StartAssignment(source: Source, target: Target) extends Instruction
    case class LowHighAssignment(source: Bot, low: Target, high: Target) extends Instruction

    def toTarget(name: String, id: String): Target = {
        val numId = id.toInt
        name match {
            case "output" => Output(numId)
            case "bot" => Bot(numId)
        }
    }

    val instructions: Seq[Instruction] = asLines(Input2016.Day10) map {
        case valueToBot(value, targetType, targetId) =>
            StartAssignment(Value(value.toInt), toTarget(targetType, targetId))
        case botGivesLowAndHigh(source, lowType, lowId, highType, highId) =>
            LowHighAssignment(Bot(source.toInt), toTarget(lowType, lowId), toTarget(highType, highId))
    }

    @inline
    def addOrInsert[A, B](map: Map[A, Seq[B]], key: A, value: B): Map[A, Seq[B]] = {
        map + (key -> (map.getOrElse(key, Seq.empty[B]) :+ value))
    }

    def addOrInsertMany[A, B](map: Map[A, Seq[B]], values: (A, B)*): Map[A, Seq[B]] = {
        values.foldLeft(map) {
            case (m, (k, v)) => addOrInsert(m, k, v)
        }
    }

    def buildStart(instructions: Seq[Instruction]): (Seq[LowHighAssignment], Map[Target, Seq[Value]]) = {
        instructions.foldLeft((Seq.empty[LowHighAssignment], Map.empty[Target, Seq[Value]])) {
            case ((instrOut, values), StartAssignment(v: Value, target)) =>
                (instrOut, addOrInsert(values, target, v))
            case ((instrOut, value), i: LowHighAssignment) =>
                (instrOut :+ i, value)
            case _ => throw new Exception("Hit illegal input")
        }
    }

    @tailrec
    def applyAll(assignments: Seq[LowHighAssignment], valueTable: Map[Target, Seq[Value]]): Map[Target, Seq[Value]] = {
        if (assignments.isEmpty) valueTable
        else {
            val ass = assignments.head
            val values = valueTable.getOrElse(ass.source, Nil)
            if (values.length == 2) {
                val newValues = addOrInsertMany(valueTable, ass.low -> values.minBy(_.value), ass.high -> values.maxBy(_.value))
                applyAll(assignments.tail, newValues)
            } else {
                applyAll(assignments.tail :+ assignments.head, valueTable)
            }
        }
    }

    val (assignments, startValues) = buildStart(instructions)
    val finalValueTable = applyAll(assignments, startValues)

    val targetValues = Set(Value(61), Value(17))
    val targetBotId = finalValueTable.find {
        case (Bot(_), values) => (targetValues -- values).isEmpty
        case _ => false
    }.map {
        case (Bot(n), _) => n
        case _ => throw new Exception("Hit illegal input")
    }.head

    part(1, targetBotId)

    val targetOutputIds = Set(0, 1, 2)
    val outputProduct = finalValueTable.filter {
        case (Output(n), _) => targetOutputIds.contains(n)
        case _ => false
    }.map {
        case (_, head :: _) => head.value
        case _ => throw new Exception("Hit illegal input")
    }.product

    part(2, outputProduct)
}
