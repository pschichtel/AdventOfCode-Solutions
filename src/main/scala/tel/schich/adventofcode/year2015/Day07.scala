package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

object Day07 extends AoCApp {

    private val RelevantWire = "a"
    private val ReplaceWire = "b"

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

    def num(s: String): Boolean = s.forall(Character.isDigit)

    def icInput(s: String): Input =
        if (num(s)) Const(s.toInt)
        else Dyn(s)

    private val wire   = "(\\w+) -> (\\w+)".r
    private val and    = "(\\w+) AND (\\w+) -> (\\w+)".r
    private val or     = "(\\w+) OR (\\w+) -> (\\w+)".r
    private val not    = "NOT (\\w+) -> (\\w+)".r
    private val lshift = "(\\w+) LSHIFT (\\d+) -> (\\w+)".r
    private val rshift = "(\\w+) RSHIFT (\\d+) -> (\\w+)".r

    type WireStates = Map[String, Int]

    def evaluate(ICs: Seq[IC]): WireStates = {

        val byOutput = ICs.groupBy(_.output)
        //val byInput = ICs.flatMap(ic => ic.inputs.collect {
        //    case Dyn(n) => (n, ic)
        //}).groupBy(_._1).view.mapValues(_.map(_._2))

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
                        case _ => throw new Exception("should not be reached")
                    }
                    case Or(l, r, o) => (l, r) match {
                        case (Dyn(nl), Dyn(nr)) if in.contains(nl) && in.contains(nr) => in + (o -> (in(nl) | in(nr)))
                        case (Const(vl), Const(vr)) => in + (o -> (vl | vr))
                        case (Dyn(nl), Const(vr)) if in contains nl => in + (o -> (in(nl) | vr))
                        case (Const(vl), Dyn(nr)) if in contains nr => in + (o -> (vl | in(nr)))
                        case _ => throw new Exception("should not be reached")
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

    override def solution: (Any, Any) = {
        val input = asLines(Input2015.Day07).toList

        val ICs: Seq[IC] = input.map {
            case wire(i, o) => Wire(icInput(i), o)
            case and(l, r, o) => And(icInput(l), icInput(r), o)
            case or(l, r, o) => Or(icInput(l), icInput(r), o)
            case not(i, o) => Not(icInput(i), o)
            case lshift(i, by, o) => LShift(icInput(i), by.toInt, o)
            case rshift(i, by, o) => RShift(icInput(i), by.toInt, o)
        }

        val relevantValue1 = evaluate(ICs)(RelevantWire)

        val newICs = ICs.map {
            case Wire(_, ReplaceWire) => Wire(Const(relevantValue1), ReplaceWire)
            case ic => ic
        }

        val relevantValue2 = evaluate(newICs)(RelevantWire)

        (relevantValue1, relevantValue2)
    }
}
