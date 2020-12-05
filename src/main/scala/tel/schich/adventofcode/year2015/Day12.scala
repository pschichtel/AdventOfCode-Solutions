package tel.schich.adventofcode.year2015

import tel.schich.adventofcode.shared.AoCApp

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Day12 extends AoCApp {

    sealed trait JsonValue
    sealed trait JsonNumber extends JsonValue
    case class JsonObject(children: Map[String, JsonValue]) extends JsonValue
    case class JsonArray(children: Seq[JsonValue]) extends JsonValue
    case class JsonString(string: String) extends JsonValue
    case class JsonFloat(value: Double) extends JsonNumber
    case class JsonInt(value: Long) extends JsonNumber

    class IncompleteParse(rest: Seq[Char]) extends Exception(s"The input was not completely parsed: $rest")
    class UnexpectedToken(c: Char) extends Exception(s"Unexpected character '$c'!")

    def parseJson(input: Seq[Char]): Try[JsonValue] = {
        try {
            val (value, rest) = parseJsonValue(input)
            if (rest.isEmpty) Success(value)
            else Failure(new IncompleteParse(rest))
        } catch {
            case e: UnexpectedToken => Failure(e)
        }
    }

    def parseJsonValue(input: Seq[Char]): (JsonValue, Seq[Char]) = {
        input.head match {
            case '{' => parseJsonObject(input)
            case '[' => parseJsonArray(input)
            case '"' => parseJsonString(input)
            case _ => parseJsonNumber(input)
        }
    }

    def parseJsonObject(input: Seq[Char]): (JsonObject, Seq[Char]) = {
        @tailrec
        def parseEntries(input: Seq[Char], children: Map[String, JsonValue]): (Map[String, JsonValue], Seq[Char]) = {

            input.head match {
                case c if c == '{' || c == ',' =>
                    val (name, afterName) = parseString(input.tail)
                    afterName.head match {
                        case ':' =>
                            val (value, rest) = parseJsonValue(afterName.tail)
                            parseEntries(rest, children + (name -> value))
                        case nc =>
                            throw new UnexpectedToken(nc)
                    }
                case '}' =>
                    (children, input.tail)
                case c =>
                    throw new UnexpectedToken(c)
            }
        }
        val (children, rest) = parseEntries(input, Map.empty)
        (JsonObject(children), rest)
    }

    def parseJsonArray(input: Seq[Char]): (JsonArray, Seq[Char]) = {
        @tailrec
        def parseArray(input: Seq[Char], children: Seq[JsonValue]): (Seq[JsonValue], Seq[Char]) = {
            input.head match {
                case c if c == '[' || c == ',' =>
                    val (value, rest) = parseJsonValue(input.tail)
                    parseArray(rest, children :+ value)
                case ']' =>
                    (children, input.tail)
                case c =>
                    throw new UnexpectedToken(c)
            }
        }

        val (children, rest) = parseArray(input, Vector.empty)
        (JsonArray(children), rest)
    }

    def parseJsonString(input: Seq[Char]): (JsonString, Seq[Char]) = {
        val (string, rest) = parseString(input)
        (JsonString(string), rest)
    }

    def parseString(input: Seq[Char]): (String, Seq[Char]) = {

        @tailrec
        def parse(input: Seq[Char], acc: String): (String, Seq[Char]) = {
            input.head match {
                case '\\' =>
                    val (char, rest) = parseEscapeSequence(input.tail)
                    parse(rest, acc + char)
                case '"' =>
                    (acc, input.tail)
                case c => parse(input.tail, acc + c)
            }
        }

        def parseEscapeSequence(input: Seq[Char]): (Char, Seq[Char]) = {
            val char = input.head match {
                case '"' => '"'
                case 'n' => '\n'
                case 't' => '\t'
                case c => throw new UnexpectedToken(c)
            }

            (char, input.tail)
        }

        input.head match {
            case '"' => parse(input.tail, "")
            case c => throw new UnexpectedToken(c)
        }
    }

    def parseJsonNumber(startInput: Seq[Char]): (JsonNumber, Seq[Char]) = {
        def parse(input: Seq[Char], acc: String, float: Boolean): (JsonNumber, Seq[Char]) = {
            if (input.isEmpty) (toObj(acc, float), input)
            else input.head match {
                case c if input == startInput && (c == '-' || c == '+') =>
                    parse(input.tail, acc + c, float)
                case c @ '.' =>
                    parse(input.tail, acc + c, float = true)
                case c if Character.isDigit(c) =>
                    parse(input.tail, acc + c, float)
                case _ =>
                    (toObj(acc, float), input)
            }
        }

        def toObj(number: String, float: Boolean): JsonNumber = {
            if (float) JsonFloat(java.lang.Double.parseDouble(number))
            else JsonInt(Integer.parseInt(number))
        }

        parse(startInput, "", float = false)
    }

    def findNumbers(o: JsonValue): Seq[Number] = {
        o match {
            case JsonInt(n) => Seq(n)
            case JsonObject(children) => children.values.flatMap(findNumbers).toSeq
            case JsonArray(children) => children.flatMap(findNumbers)
            case _ => Nil
        }
    }

    def removeObjectsWith(o: JsonValue, e: JsonValue): JsonValue = {

        o match {
            case JsonObject(children) =>
                if (children.values.exists(_ == e)) JsonObject(Map.empty)
                else JsonObject(children.view.mapValues(v => removeObjectsWith(v, e)).toMap)
            case JsonArray(children) =>
                JsonArray(children.map(v => removeObjectsWith(v, e)))
            case leaf => leaf
        }

    }

    parseJson(inputText) match {
        case Success(tree) =>
            val sumOfNumbers = findNumbers(tree).map(_.intValue).sum
            part(1, sumOfNumbers)

            val sumOfNonRedNumbers = findNumbers(removeObjectsWith(tree, JsonString("red"))).map(_.intValue).sum
            part(2, sumOfNonRedNumbers)
        case Failure(e) =>
            e.printStackTrace()
    }

}
