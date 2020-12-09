package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._

import scala.annotation.tailrec

object Day08 extends AoCApp {

    sealed trait Instr
    final case class NoOperation(v: Int) extends Instr
    final case class Accumulate(v: Int) extends Instr
    final case class Jump(v: Int) extends Instr

    case class Block(start: Int, end: Int, jumpTo: Int)

    @tailrec
    def executeUntil(program: Array[Instr], pc: Int, end: Int, acc: Int): Int = {
        if (pc == end) acc
        else program(pc) match {
            case NoOperation(_) => executeUntil(program, pc + 1, end, acc)
            case Accumulate(v) => executeUntil(program, pc + 1, end, acc + v)
            case Jump(v) => executeUntil(program, pc + v, end, acc)
        }
    }

    def extractForwardLoopBody(reversePath: List[Block]): List[Block] = {
        @tailrec
        def find(until: Block, backwards: List[Block], forwards: List[Block]): List[Block] = {
            if (backwards.head == until) backwards.head :: forwards
            else find(until, backwards.tail, backwards.head :: forwards)
        }

        find(reversePath.head, reversePath.tail, Nil)
    }

    @tailrec
    def findReachedBy(reachable: Set[Block], predecessorLookup: Map[Block, Set[Block]]): Set[Block] = {
        val newReachable = reachable ++ reachable.flatMap(b => predecessorLookup.getOrElse(b, Set.empty))
        if (newReachable != reachable) findReachedBy(newReachable, predecessorLookup)
        else reachable
    }

    def findLoop(start: Block, successorLookup: Map[Block, Block]): List[Block] = {
        @tailrec
        def findPath(path: List[Block], known: Set[Block]): List[Block] = {
            if (known.contains(path.head)) path
            else successorLookup.get(path.head) match {
                case Some(value) =>findPath(value :: path, known + path.head)
                case None => path
            }
        }

        findPath(start :: Nil, Set.empty)
    }

    def findBlocks(program: Array[Instr]): Set[Block] = {
        @tailrec
        def search(pc: Int, blockStart: Int, blocks: Set[Block]): Set[Block] = {
            if (pc >= program.length) blocks + Block(blockStart, pc - 1, program.length)
            else program(pc) match {
                case Jump(v) => search(pc + 1, pc + 1, blocks + Block(blockStart, pc, pc + v))
                case _ => search(pc + 1, blockStart, blocks)
            }
        }

        search(0, 0, Set.empty)
    }

    def findCorrectableInstructions(program: Array[Instr], pcToBlock: Map[Int, Block], blocks: Set[Block], predecessorLookup: Map[Block, Set[Block]], loopBody: List[Block]): Set[Int] = {
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

    def buildAdjacentLookups(program: Array[Instr], pcToBlock: Map[Int, Block], blocks: Set[Block]): (Map[Block, Block], Map[Block, Set[Block]]) = {
        @tailrec
        def build(blocks: Set[Block], successors: Set[(Block, Block)], predecessors: Set[(Block, Block)]): (Map[Block, Block], Map[Block, Set[Block]]) = {
            if (blocks.isEmpty) (successors.toMap, predecessors.groupMap(_._1)(_._2))
            else {
                val block = blocks.head
                if (block.jumpTo >= program.length) build(blocks.tail, successors, predecessors)
                else {
                    val successor = pcToBlock(block.jumpTo)
                    build(blocks.tail, successors + (block -> successor), predecessors + (successor -> block))
                }
            }
        }

        build(blocks, Set.empty, Set.empty)
    }

    val parseNoOperation = parseString("nop ").ignoreAndThen(parseWholeNumber).map(i => NoOperation(i.toInt))
    val parseAccumulator = parseString("acc ").ignoreAndThen(parseWholeNumber).map(i => Accumulate(i.toInt))
    val parseJump = parseString("jmp ").ignoreAndThen(parseWholeNumber).map(i => Jump(i.toInt))

    val parseOp = parseSelector[Instr](Seq(parseNoOperation, parseAccumulator, parseJump))
    val parseInput = parseAllSeparated(parseOp, parseLineBreak)

    val program = input(parseInput).toArray

    val blocks = findBlocks(program)
    val pcToBlock = (for {
        block <- blocks
        pc <- block.start to block.`end`
    } yield (pc, block)).toMap

    val (successorLookup, predecessorLookup) = buildAdjacentLookups(program, pcToBlock, blocks)

    val start = pcToBlock(0)
    val backwardsPathFromLoop = findLoop(start, successorLookup)

    part(1, executeUntil(program, 0, backwardsPathFromLoop.tail.head.`end`, 0))

    val forwardsLoopBody = extractForwardLoopBody(backwardsPathFromLoop)
    val correctableInstructions = findCorrectableInstructions(program, pcToBlock, blocks, predecessorLookup, forwardsLoopBody)
    val pcToCorrect = correctableInstructions.head
    val correctedProgram = program(pcToCorrect) match {
        case NoOperation(v) => program.updated(pcToCorrect, Jump(v))
        case Jump(v) => program.updated(pcToCorrect, NoOperation(v))
        case _ => program
    }

    part(2, executeUntil(correctedProgram, 0, correctedProgram.length, 0))

    // approaches for part 2
    // 1. depth-first search trying to mutate the program (can reuse partially executed programs)
    // 2. linearly scan through the program flipping each instruction once
    // 3. analyse the program flow (extract looping sub-program and test for mutations into paths that terminate)
}
