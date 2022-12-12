package tel.schich.adventofcode.year2022

import tel.schich.adventofcode.shared.Parser.{parseAllSeparated, parseIndentedLine, parseLine, parseLineBreak, parseNaturalNumber, parseRepeated, parseSelector, parseString, parseWhile}
import tel.schich.adventofcode.shared.{AoCApp, Parser}
import tel.schich.adventofcode.year2020.Day13
import tel.schich.adventofcode.year2022.Expression.{Add, Multiply, Number, Old}

import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, Queue}

enum Expression {
    case Add(a: Expression, b: Expression)
    case Multiply(a: Expression, b: Expression)
    case Number(n: Long)
    case Old
}

object Day11 extends AoCApp {

    def parseSymbol = parseSelector(Seq(parseNaturalNumber.map(Expression.Number.apply), parseString("old").map(_ => Expression.Old)))

    def parseMultiply = for {
        a <- parseSymbol
        _ <- parseString(" * ")
        b <- parseSymbol
    } yield Expression.Multiply(a, b)

    def parseAdd = for {
        a <- parseSymbol
        _ <- parseString(" + ")
        b <- parseSymbol
    } yield Expression.Add(a, b)

    def parseMonkeyId = parseString("Monkey ").ignoreAndThen(parseNaturalNumber).andThenIgnore(parseString(":"))
    def parseStartingItems = parseString("Starting items: ").ignoreAndThen(parseAllSeparated(parseNaturalNumber, parseString(", ")))
    def parseOperation = parseString("Operation: new = ").ignoreAndThen(parseSelector(Seq(parseAdd, parseMultiply)))
    def parseTest = parseString("Test: divisible by ").ignoreAndThen(parseNaturalNumber)
    def parseAction(value: Boolean) = parseString(s"If $value: throw to monkey ").ignoreAndThen(parseNaturalNumber)

    case class Monkey(id: Long, startItems: Seq[Long], op: Expression, divisibleBy: Long, onTrue: Long, onFalse: Long)

    def parseMonkey = for {
        monkey <- parseLine(parseMonkeyId)
        startItems <- parseIndentedLine(parseStartingItems, "  ", 1)
        op: Expression <- parseIndentedLine(parseOperation, "  ", 1)
        divisibleBy <- parseIndentedLine(parseTest, "  ", 1)
        onTrue <- parseIndentedLine(parseAction(true), "  ", 2)
        onFalse <- parseIndentedLine(parseAction(false), "  ", 2)
    } yield Monkey(monkey, startItems, op, divisibleBy, onTrue, onFalse)

    def parseMonkeys = parseAllSeparated(parseMonkey, parseLineBreak)

    val monkeys = parse(Input2022.Day11, parseMonkeys)

    def applyExpression(old: Long, expr: Expression, divider: Long): Long = expr match {
        case Multiply(a, prime) => applyExpression(old, a, divider) * applyExpression(old, prime, divider)
        case Add(a, b) => applyExpression(old, a, divider) + applyExpression(old, b, divider)
        case Number(n) => n
        case Old => old
    }

    def monkeyTurn(items: Queue[Long], monkey: Monkey, monkeyItems: Map[Long, Queue[Long]], divider: Long, remainder: Long): (Map[Long, Queue[Long]], Long) = {
        if (items.isEmpty) (monkeyItems, 0)
        else {
            val newItems = items.foldLeft(monkeyItems) { (thrown, item) =>
                val rawValue = applyExpression(item, monkey.op, divider)
                val newValue = (rawValue % remainder) / divider
                val targetMonkey = if (newValue % monkey.divisibleBy == 0) monkey.onTrue else monkey.onFalse
                thrown.updated(targetMonkey, thrown.getOrElse(targetMonkey, Queue.empty).enqueue(newValue))
            }
            (newItems, items.size.toLong)
        }
    }

    @tailrec
    private def round(monkeys: Seq[Monkey], items: Map[Long, Queue[Long]], inspectCount: Map[Long, Long], divider: Long, remainder: Long): (Map[Long, Queue[Long]], Map[Long, Long]) = {
        if (monkeys.isEmpty) {
            (items, inspectCount)
        } else {
            val monkey = monkeys.head
            val monkeyItems = items.getOrElse(monkey.id, Queue.empty)
            val (newItems, inspections) = monkeyTurn(monkeyItems, monkey, items.removed(monkey.id), divider, remainder)
            val newInspectCount = inspectCount.updatedWith(monkey.id) { x => Some(x.getOrElse(0L) + inspections) }
            round(monkeys.tail, newItems, newInspectCount, divider, remainder)
        }
    }

    def doStuff(divider: Long, iterations: Int): Long = {
        val monkeyItems = monkeys.map(m => (m.id, Queue.from(m.startItems))).toMap

        val checkRemainder = monkeys.map(_.divisibleBy).product

        val (_, inspectCount) = (1 to iterations).foldLeft((monkeyItems, Map.empty[Long, Long])) {
            case ((iterationItems, inspectCount), _) =>
                round(monkeys, iterationItems, inspectCount, divider, checkRemainder)
        }

        inspectCount.values.toSeq.sorted.reverse.take(2).product
    }

    override def solution: (Any, Any) = (doStuff(3, 20), doStuff(1, 10000))
}
