import scala.io.Source
import scala.math._

object Day7 extends AoCApp {
    println("Day  7")

    val input = sourceFromCP("day7.txt").mkString.trim.split('\n').map(_.trim).toList
    val RelevantWire = "a"
    val ReplaceWire = "b"

    sealed trait Input

    case class Const(value: Int) extends Input

    case class Dyn(name: String) extends Input

    trait IC {
        def output: String

        def inputs: Set[Input]
    }

    trait Unary extends IC {
        def input: Input

        lazy val inputs = Set(input)
    }

    trait Binary extends IC {
        def left: Input

        def right: Input

        lazy val inputs = Set(left, right)
    }

    case class Wire(input: Input, output: String) extends Unary

    case class And(left: Input, right: Input, output: String) extends Binary

    case class Or(left: Input, right: Input, output: String) extends Binary

    case class Not(input: Input, output: String) extends Unary

    case class LShift(input: Input, by: Int, output: String) extends Unary

    case class RShift(input: Input, by: Int, output: String) extends Unary

    def num(s: String) = s.forall(Character.isDigit)

    def input(s: String): Input =
        if (num(s)) Const(s.toInt)
        else Dyn(s)

    val wire = raw"(\w+) -> (\w+)".r
    val and = raw"(\w+) AND (\w+) -> (\w+)".r
    val or = raw"(\w+) OR (\w+) -> (\w+)".r
    val not = raw"NOT (\w+) -> (\w+)".r
    val lshift = raw"(\w+) LSHIFT (\d+) -> (\w+)".r
    val rshift = raw"(\w+) RSHIFT (\d+) -> (\w+)".r

    val ICs: Seq[IC] = input.map {
        case wire(i, o) => Wire(input(i), o)
        case and(l, r, o) => And(input(l), input(r), o)
        case or(l, r, o) => Or(input(l), input(r), o)
        case not(i, o) => Not(input(i), o)
        case lshift(i, by, o) => LShift(input(i), by.toInt, o)
        case rshift(i, by, o) => RShift(input(i), by.toInt, o)
    }

    type WireStates = Map[String, Int]

    def evaluate(ICs: Seq[IC]): WireStates = {

        val byOutput = ICs.groupBy(_.output)
        val byInput = ICs.flatMap(ic => ic.inputs.collect {
            case Dyn(n) => (n, ic)
        }).groupBy(_._1).mapValues(_.map(_._2))

        def apply(ic: IC, wireStates: WireStates = Map.empty, applied: Set[IC] = Set.empty, stack: List[IC] = Nil): (WireStates, Set[IC]) = {

            if (applied.contains(ic)) (wireStates, applied)
            else {
                if (stack.contains(ic)) throw new Exception(s"Cycle!\nCurrent IC: $ic\nStack: $stack\nAlready applied: $applied\nKnown states$wireStates")

                val stackWithMe = ic :: stack

                val (in, newlyApplied) = ic.inputs.collect { case Dyn(n) => byOutput.getOrElse(n, Nil) }.flatten.foldLeft((wireStates, applied)) {
                    case ((states, applied), ic) => apply(ic, states, applied, stackWithMe)
                }

                val out = ic match {
                    case Wire(Const(v), o) => in + (o -> v)
                    case Wire(Dyn(n), o) if in contains n => in + (o -> in(n))
                    case And(l, r, o) => (l, r) match {
                        case (Dyn(nl), Dyn(nr)) if in.contains(nl) && in.contains(nr) => in + (o -> (in(nl) & in(nr)))
                        case (Const(vl), Const(vr)) => in + (o -> (vl & vr))
                        case (Dyn(nl), Const(vr)) if in contains nl => in + (o -> (in(nl) & vr))
                        case (Const(vl), Dyn(nr)) if in contains nr => in + (o -> (vl & in(nr)))
                    }
                    case Or(l, r, o) => (l, r) match {
                        case (Dyn(nl), Dyn(nr)) if in.contains(nl) && in.contains(nr) => in + (o -> (in(nl) | in(nr)))
                        case (Const(vl), Const(vr)) => in + (o -> (vl | vr))
                        case (Dyn(nl), Const(vr)) if in contains nl => in + (o -> (in(nl) | vr))
                        case (Const(vl), Dyn(nr)) if in contains nr => in + (o -> (vl | in(nr)))
                    }
                    case Not(Const(v), o) => in + (o -> (~v & 0xFFFF))
                    case Not(Dyn(n), o) if in contains n => in + (o -> (~in(n) & 0xFFFF))
                    case LShift(Const(v), by, o) => in + (o -> ((v << by) & 0xFFFF))
                    case LShift(Dyn(n), by, o) if in contains n => in + (o -> ((in(n) << by) & 0xFFFF))
                    case RShift(Const(v), by, o) => in + (o -> ((v >> by) & 0xFFFF))
                    case RShift(Dyn(n), by, o) if in contains n => in + (o -> ((in(n) >> by) & 0xFFFF))
                    case _ => in
                }

                (out, newlyApplied + ic)

            }
        }

        val (states, _) = ICs.foldLeft((Map.empty[String, Int], Set.empty[IC])) {
            case ((states, applied), ic) =>
                apply(ic, states, applied)
        }

        states
    }

    val relevantValue1 = evaluate(ICs)(RelevantWire)

    println(s"Part 1: $relevantValue1")

    val newICs = ICs.map {
        case Wire(_, ReplaceWire) => Wire(Const(relevantValue1), ReplaceWire)
        case ic => ic
    }

    val relevantValue2 = evaluate(newICs)(RelevantWire)

    println(s"Part 2: $relevantValue2")

}