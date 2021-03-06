package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.{AoCApp, StringSlice}
import tel.schich.adventofcode.shared.Parser._

import scala.annotation.tailrec

object Day07 extends AoCApp {

    type Bag = (StringSlice, StringSlice)
    type CountedBag = (Long, Bag)
    type BagWithBags = (Bag, Seq[CountedBag])

    private val parseBag = for {
        modifier <- parseWord
        _ <- parseSpaces
        color <- parseWord
        _ <- parseString(" bag")
        _ <- parseString("s").?
    } yield (modifier, color)

    private val parseBagWithCount = for {
        count <- parseNaturalNumber
        _ <- parseSpaces
        bag <- parseBag
    } yield (count, bag)

    private val parseContainedBags = parseAllSeparated(parseBagWithCount, parseString(", ")).or(parseString("no other bags")).map {
        case Left(bags) => bags
        case _ => Nil
    }

    private val parseStatement = for {
        bag <- parseBag
        _ <- parseString(" contain ")
        containedBags <- parseContainedBags
        _ <- parseString(".")
    } yield (bag, containedBags)

    private val parseInput = parseAllSeparated(parseStatement, parseLineBreak)

    def findBagsContaining(needle: Bag, haystack: Seq[BagWithBags]): Set[Bag] = {

        @tailrec
        def fixPoint(bags: Set[Bag], bagsLeft: Seq[BagWithBags]): Set[Bag] = {
            val (newBags, remainder) = bagsLeft.partition(_._2.exists(cb => bags.contains(cb._2)))

            if (newBags.isEmpty) bags
            else fixPoint(bags ++ newBags.map(_._1), remainder)
        }

        fixPoint(Set(needle), haystack) - needle
    }

    def countChildren(bag: Bag, lookup: Map[Bag, Seq[CountedBag]]): Long = {
        val children = lookup.getOrElse(bag, Nil)
        children.foldLeft(0L) {
            case (count, (bagCount, bag)) => count + bagCount + bagCount * countChildren(bag, lookup)
        }
    }

    override def solution: (Any, Any) = {
        val allThemBags: Seq[BagWithBags] = parse(Input2020.Day07, parseInput)

        val myBag = (StringSlice("shiny"), StringSlice("gold"))

        val part1 = findBagsContaining(myBag, allThemBags).size

        val childrenLookup: Map[Bag, Seq[CountedBag]] = allThemBags.toMap
        val part2 = countChildren(myBag, childrenLookup)

        (part1, part2)
    }
}
