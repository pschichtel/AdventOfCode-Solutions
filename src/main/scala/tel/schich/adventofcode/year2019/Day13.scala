package tel.schich.adventofcode.year2019

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.year2019.Day02._
import tel.schich.adventofcode.year2019.Day09.instructions

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day13 extends AoCApp {

    sealed trait GameResult
    case object Error extends GameResult
    case object Incomplete extends GameResult
    case class Complete(score: Long) extends GameResult

    val program = Day02.parseProgram(Input2019.Day13)

    val output = runProgram(instructions, program, Nil).output

    val tileMap = output.grouped(3).foldLeft(Map.empty[(Long, Long), Long]) {
        case (map, List(x, y, kind)) => map.updated((x, y), kind)
        case _ => throw new Exception("should not be reached")
    }

    part(1, tileMap.count(_._2 == 2))

    val programWithCoins = Day02.patch(program, 0L -> 2L)
    val stateWithCoins = initProgram(instructions, programWithCoins, Nil)


    part(2, searchBestInput(Queue(stateWithCoins), -1))

    @tailrec
    def searchBestInput(states: Queue[ProgramState], currentHighScore: Long): Long = {
        if (states.length % 100 == 0) {
            println(states.length)
        }
        if (states.isEmpty) currentHighScore
        else {
            val (state, restOfStates) = states.dequeue

            //println(input)
            runGame(state) match {
                case Error =>
                    println("failed")
                    searchBestInput(restOfStates, currentHighScore)
                case Incomplete =>
                    //println(s"incomplete: $value (queue length ${input.length + 3})")
                    val more = Seq(-1, 0, 1).map(action => state.continue(input = state.input :+ action))
                    searchBestInput(restOfStates.enqueueAll(more), currentHighScore)
                case Complete(score) =>
                    //println("complete")
                    if (score > currentHighScore) {
                        println(s"new score: $score")
                        //searchBestInput(startState, newQueue.enqueueAll(Seq(value :+ -1, value :+ 0, value :+ 1)), score)
                        score
                    } else searchBestInput(restOfStates, currentHighScore)
            }
        }
    }

    def runGame(startState: ProgramState): GameResult = {
        @tailrec
        def run(state: ProgramState, score: Long): GameResult = {
            val newState = state.runInstruction()
            newState.status match {
                case Ready =>
                    newState.output match {
                        case List(-1L, 0L, s) => run(newState.continue(output = Nil), s)
                        case l if l.nonEmpty => run(newState.continue(output = Nil), score)
                        case _ => run(newState, score)
                    }
                case SuccessfullyCompleted =>
                    Complete(score)
                case Failed(e) =>
                    e.printStackTrace(System.err)
                    Error
                case RequiredMoreInput =>
                    Incomplete
            }
        }

        run(startState, -1)
    }

}
