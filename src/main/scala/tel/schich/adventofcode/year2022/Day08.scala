package tel.schich.adventofcode.year2022

import tel.schich.adventofcode.shared.Parser.{parseLines, parseNaturalNumber, parseSelector, parseString, parseWhile}
import tel.schich.adventofcode.shared.{AoCApp, Parser}
import tel.schich.adventofcode.year2022.Day08.Command.ChDir
import tel.schich.adventofcode.year2022.Day08.ListingEntry.File
import tel.schich.adventofcode.year2022.Day08.LogLine.{Listing, Prompt}

import scala.annotation.tailrec

object Day08 extends AoCApp {

    enum Command {
        case ChDir(val path: String)
        case ListDir
    }

    enum ListingEntry {
        case File(val name: String, val size: Long)
        case Directory(val name: String)
    }

    enum LogLine {
        case Prompt(command: Command)
        case Listing(listingEntry: ListingEntry)
    }

    private val parseInput = {
        val parseCd: Parser[Command] = parseString("cd ").ignoreAndThen(parseWhile(!_.isWhitespace))
            .map(name => Command.ChDir(name.asString))
        val parseLs = parseString("ls").map(_ => Command.ListDir)
        val parseCommand = parseString("$ ").ignoreAndThen(parseSelector[Command](Seq(parseCd, parseLs)))

        val parseFileEntry = for {
            size <- parseNaturalNumber
            _ <- parseWhile(_.isWhitespace)
            name <- parseWhile(!_.isWhitespace)
        } yield ListingEntry.File(name.asString, size)
        val parseDirEntry = parseString("dir ").ignoreAndThen(parseWhile(!_.isWhitespace))
            .map(name => ListingEntry.Directory(name.asString))
        val parseListingEntry = parseSelector[ListingEntry](Seq(parseFileEntry, parseDirEntry))
        val parseLogLine = parseSelector[LogLine](Seq(parseCommand.map(c => LogLine.Prompt(c)), parseListingEntry.map(e => LogLine.Listing(e))))
        parseLines(parseLogLine)
    }

    private val input = parse(Input2022.Day08, parseInput).toList

    private case class FileSize(path: Vector[String], size: Long)

    @tailrec
    private def resolvePaths(currentDir: Vector[String], remainingEntries: List[LogLine], fileSizes: Vector[FileSize]): Vector[FileSize] = {
        if (remainingEntries.isEmpty) fileSizes
        else {
            remainingEntries.head match {
                case Prompt(ChDir("/")) => resolvePaths(Vector.empty, remainingEntries.tail, fileSizes)
                case Prompt(ChDir("..")) => resolvePaths(currentDir.dropRight(1), remainingEntries.tail, fileSizes)
                case Prompt(ChDir(folder)) => resolvePaths(currentDir :+ folder, remainingEntries.tail, fileSizes)
                case Listing(File(_, size)) => resolvePaths(currentDir, remainingEntries.tail, fileSizes :+ FileSize(currentDir, size))
                case _ => resolvePaths(currentDir, remainingEntries.tail, fileSizes)
            }
        }
    }

    private val directorySizes = resolvePaths(Vector.empty, input, Vector.empty)
        .flatMap { fileSize =>
            (0 to fileSize.path.size).map { i =>
                FileSize(fileSize.path.take(i), fileSize.size)
            }
        }
        .foldLeft(Map.empty[Vector[String], Long]) { (sizes, size) =>
            sizes.updatedWith(size.path) { v => Some(v.getOrElse(0L) + size.size) }
        }

    private val diskSize = 70_000_000L
    private val requiredSpace = 30_000_000L
    private val usedSpace = directorySizes(Vector.empty)
    private val freeSpace = diskSize - usedSpace


    override def solution: (Any, Any) =
        (directorySizes.values.filter(_ <= 100_000).sum, directorySizes.values.filter(freeSpace + _ > requiredSpace).min)
}
