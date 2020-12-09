package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

import scala.annotation.tailrec

object Day08 extends AoCApp {

    sealed trait Op
    final case class NoOperation(v: Int) extends Op
    final case class Accumulate(v: Int) extends Op
    final case class Jump(v: Int) extends Op

    case class CodeBlock(start: Int, end: Int, jumpTo: Int)

    @tailrec
    def executeAndDetectLoop(program: Array[Op], pc: Int, end: Int, acc: Int): Int = {
        if (pc == end) acc
        else program(pc) match {
            case NoOperation(_) => executeAndDetectLoop(program, pc + 1, end, acc)
            case Accumulate(v) => executeAndDetectLoop(program, pc + 1, end, acc + v)
            case Jump(v) => executeAndDetectLoop(program, pc + v, end, acc)
        }
    }

    def extractForwardLoopBody(reversePath: List[CodeBlock]): List[CodeBlock] = {
        @tailrec
        def find(until: CodeBlock, backwards: List[CodeBlock], forwards: List[CodeBlock]): List[CodeBlock] = {
            if (backwards.head == until) backwards.head :: forwards
            else find(until, backwards.tail, backwards.head :: forwards)
        }

        find(reversePath.head, reversePath.tail, Nil)
    }

    @tailrec
    def findReachedBy(reachable: Set[CodeBlock], predecessorLookup: Map[CodeBlock, Set[CodeBlock]]): Set[CodeBlock] = {
        val newReachable = reachable ++ reachable.flatMap(b => predecessorLookup.getOrElse(b, Set.empty))
        if (newReachable != reachable) findReachedBy(newReachable, predecessorLookup)
        else reachable
    }

    def findLoop(start: CodeBlock, successorLookup: Map[CodeBlock, CodeBlock]): List[CodeBlock] = {
        @tailrec
        def findPath(path: List[CodeBlock], known: Set[CodeBlock]): List[CodeBlock] = {
            if (known.contains(path.head)) path
            else successorLookup.get(path.head) match {
                case Some(value) =>findPath(value :: path, known + path.head)
                case None => path
            }
        }

        findPath(start :: Nil, Set.empty)
    }

    def findBlocks(program: Array[Op]): Set[CodeBlock] = {
        @tailrec
        def search(pc: Int, blockStart: Int, blocks: Set[CodeBlock]): Set[CodeBlock] = {
            if (pc >= program.length) blocks + CodeBlock(blockStart, pc - 1, program.length)
            else program(pc) match {
                case Jump(v) => search(pc + 1, pc + 1, blocks + CodeBlock(blockStart, pc, pc + v))
                case _ => search(pc + 1, blockStart, blocks)
            }
        }

        search(0, 0, Set.empty)
    }

    def findCorrectableInstructions(program: Array[Op], pcToBlock: Map[Int, CodeBlock], blocks: Set[CodeBlock], predecessorLookup: Map[CodeBlock, Set[CodeBlock]], loopBody: List[CodeBlock]): Set[Int] = {
        val ends = blocks.filter(_.jumpTo >= program.length)
        val terminatingBlocks = findReachedBy(ends, predecessorLookup)
        loopBody
            .flatMap(b => (b.start to b.`end`).map(pc => (pc, program(pc))))
            .flatMap {
                case (pc, NoOperation(v)) =>
                    val possibleTarget = pc + v
                    pcToBlock.get(possibleTarget) match {
                        case Some(value) if terminatingBlocks.contains(value) => Some(pc)
                        case _ => None
                    }
                case (pc, Jump(_)) =>
                    val nextBlock = pcToBlock(pc + 1)
                    if (terminatingBlocks.contains(nextBlock)) Some(pc)
                    else None
                case _ => None
            }
            .toSet
    }

    val parseNoOperation = parseString("nop ").ignoreAndThen(parseWholeNumber).map(i => NoOperation(i.toInt))
    val parseAccumulator = parseString("acc ").ignoreAndThen(parseWholeNumber).map(i => Accumulate(i.toInt))
    val parseJump = parseString("jmp ").ignoreAndThen(parseWholeNumber).map(i => Jump(i.toInt))

    val parseOp = parseSelector[Op](Seq(parseNoOperation, parseAccumulator, parseJump))
    val parseInput = parseAllSeparated(parseOp, parseLineBreak)

    val program = input(parseInput).toArray

    val blocks = findBlocks(program)
    val pcToBlock = (for {
        block <- blocks
        pc <- block.start to block.`end`
    } yield (pc, block)).toMap

    val (successorLookup, predecessors) = blocks.foldLeft((Map.empty[CodeBlock, CodeBlock], Set.empty[(CodeBlock, CodeBlock)])) {
        case (acc, block) if block.jumpTo >= program.length => acc
        case ((forward, backward), block) =>
            val next = pcToBlock(block.jumpTo)
            (forward + (block -> next), backward + (next -> block))
    }

    val predecessorLookup = predecessors.groupMap(_._1)(_._2)

    val start = pcToBlock(0)

    val backwardsPathFromLoop = findLoop(start, successorLookup)
    part(1, executeAndDetectLoop(program, 0, backwardsPathFromLoop.tail.head.`end`, 0))

    val forwardsLoopBody = extractForwardLoopBody(backwardsPathFromLoop)


    val correctableInstructions = findCorrectableInstructions(program, pcToBlock, blocks, predecessorLookup, forwardsLoopBody)
    val pcToCorrect = correctableInstructions.head
    val correctedProgram = program(pcToCorrect) match {
        case NoOperation(v) => program.updated(pcToCorrect, Jump(v))
        case Jump(v) => program.updated(pcToCorrect, NoOperation(v))
        case _ => program
    }

    part(2, executeAndDetectLoop(correctedProgram, 0, correctedProgram.length, 0))
}
