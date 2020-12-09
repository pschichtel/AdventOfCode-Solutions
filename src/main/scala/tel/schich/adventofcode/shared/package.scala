package tel.schich.adventofcode

import tel.schich.adventofcode.shared.Parser.parseRepeated

import scala.annotation.tailrec
import scala.collection.IndexedSeqView

package object shared {

    class StringSlice(private val source: Array[Char], private val offset: Int, val length: Int)
        extends (Int => Char)
            with CharSequence {

        private lazy val hash =
            (offset until (offset + length)).foldLeft(1) { (result, element) =>
                31 * result * source(element)
            }

        def headOption: Option[Char] = if (length > 0) Some(source(offset)) else None
        override def apply(i: Int): Char = source(offset + i)

        def slice(n: Int): StringSlice = new StringSlice(source, offset, n)
        def drop(n: Int): StringSlice = new StringSlice(source, offset + n, length - n)

        def startsWith(s: String): Boolean =
            if (s.length > length) false
            else s.indices.forall(i => s.charAt(i) == source(offset + i))

        def countPrefix(p: Char => Boolean): Int = {
            @tailrec
            def count(i: Int): Int = {
                if (i >= source.length) i
                else if (p(source(i))) count(i + 1)
                else i
            }

            count(offset) - offset
        }

        def view: IndexedSeqView[Char] = source.view.slice(offset, offset + length)

        def asString = new String(source, offset, length)

        override def equals(other: Any): Boolean = other match {
            case that: StringSlice =>
                if (that.length != length) false
                else (0 until length).forall(i => apply(i) == that.apply(i))
            case _ => false
        }

        override def hashCode(): Int = hash

        override def charAt(index: Int): Char = apply(index)

        override def subSequence(start: Int, `end`: Int): CharSequence =
            new StringSlice(source, offset + start, length - start - (length - `end`))
    }

    object StringSlice {
        def apply(s: String): StringSlice = new StringSlice(s.toCharArray, 0, s.length)
    }

    trait Parser[A] extends (StringSlice => ParseResult[A]) { self =>

        def map[B](f: A => B): Parser[B] = input => apply(input).map(f)

        def flatMap[B](f: A => Parser[B]): Parser[B] = input => {
            apply(input) match {
                case ParseResult.Success(value, rest) => f(value).apply(rest)
                case ParseResult.Error(msg, rest) => ParseResult.Error(msg, rest)
            }
        }

        def ignoreAndThen[B](other: Parser[B]): Parser[B] = flatMap(_ => other)

        def andThenIgnore(other: Parser[_]): Parser[A] = flatMap(v => other.map(_ => v))

        def or[B](other: Parser[B]): Parser[Either[A, B]] = input => {
            apply(input) match {
                case ParseResult.Success(value, rest) => ParseResult.Success(Left(value), rest)
                case ParseResult.Error(_, _) => other(input).map(b => Right(b))
            }
        }

        def ? : Parser[Option[A]] = input => apply(input) match {
            case ParseResult.Success(value, rest) => ParseResult.Success(Some(value), rest)
            case ParseResult.Error(_, _) => ParseResult.Success(None, input)
        }

        def repeated(n: Int): Parser[Seq[A]] = parseRepeated(n, this)
    }

    object Parser {

        def noop[T](value: T): Parser[T] = input => ParseResult.Success(value, input)

        def check(predicate: StringSlice => Boolean): Parser[Boolean] =
            input => ParseResult.Success(predicate(input), input)

        def check(string: String): Parser[Boolean] = check(i => i.startsWith(string))

        def parseString(s: String): Parser[StringSlice] = input => {
            if (input.startsWith(s)) ParseResult.Success(input.slice(s.length), input.drop(s.length))
            else ParseResult.Error(s"Did not match string $s", input)
        }

        def parseOneOf(s: Seq[Char]): Parser[Char] = parseOne(c => s.contains(c))

        def parseOne(predicate: Char => Boolean): Parser[Char] = input => input.headOption match {
            case Some(c) if predicate(c) => ParseResult.Success(c, input.drop(1))
            case _ => ParseResult.Error("nothing found!", input)
        }

        def parseWhile(predicate: Char => Boolean): Parser[StringSlice] = input => {
            val prefixLength = input.countPrefix(predicate)
            ParseResult.Success(input.slice(prefixLength), input.drop(prefixLength))
        }

        def parseAtLeastOnce(predicate: Char => Boolean): Parser[StringSlice] = input => {
            val count = input.countPrefix(predicate)
            if (count == 0) ParseResult.Error("not enough input!", input)
            else ParseResult.Success(input.slice(count), input.drop(count))
        }

        def parseWhitespace: Parser[StringSlice] = parseWhile(_.isWhitespace)

        def parseSpaces: Parser[StringSlice] = parseAtLeastOnce(c => c.isWhitespace && c != '\r' && c != '\n')

        def parseLineBreak: Parser[StringSlice] = input => {
            if (input.startsWith("\r\n")) ParseResult.Success(input.slice(2), input.drop(2))
            else if (input.startsWith("\r") || input.startsWith("\n")) ParseResult.Success(input.slice(1), input.drop(1))
            else ParseResult.Error("no linebreak found!", input)
        }

        def parseChar: Parser[Char] = input => {
            input.headOption match {
                case Some(c) => ParseResult.Success(c, input.drop(1))
                case _ => ParseResult.Error("No more input", input)
            }
        }

        def parseRepeated[A](n: Int, parser: Parser[A]): Parser[Seq[A]] = input => {
            @tailrec
            def loop(input: StringSlice, n: Int, carry: Seq[A]): ParseResult[Seq[A]] = {
                if (n == 0) ParseResult.Success(carry, input)
                else {
                    parser(input) match {
                        case ParseResult.Success(value, rest) => loop(rest, n - 1, carry :+ value)
                        case ParseResult.Error(error, rest) => ParseResult.Error(error, rest)
                    }
                }
            }

            loop(input, n, Vector.empty)
        }

        def parseAll[A](parser: Parser[A]): Parser[Seq[A]] = input => {
            @tailrec
            def loop(input: StringSlice, carry: Seq[A]): ParseResult[Seq[A]] = {
                parser(input) match {
                    case ParseResult.Success(value, rest) => loop(rest, carry :+ value)
                    case ParseResult.Error(_, _) => ParseResult.Success(carry, input)
                }
            }

            loop(input, Vector.empty)
        }

        def parseAllSeparated[A](parser: Parser[A], separatorParser: Parser[_]): Parser[Seq[A]] =
            parser.flatMap(first => parseAll(separatorParser.ignoreAndThen(parser)).map(first +: _))

        def parseNaturalNumber: Parser[Long] = parseOne(_.isDigit).flatMap {
            case '0' => noop(0L)
            case firstDigit => parseWhile(_.isDigit).map { digits =>
                digits.view.foldLeft((firstDigit - '0').toLong)((num, digit) => num * 10 + (digit - '0'))
            }
        }

        def parseWholeNumber: Parser[Long] = parseOneOf("+-").?.flatMap { sign =>
            parseNaturalNumber.map { n =>
                sign match {
                    case Some('-') => -n
                    case _ => n
                }
            }
        }

        def parseSelector[A](parsers: Seq[Parser[_ <: A]]): Parser[A] = input => {
            @tailrec
            def tryParse(parsersLeft: Seq[Parser[_ <: A]]): ParseResult[A] = parsersLeft match {
                case Nil => ParseResult.Error("no more parser to try!", input)
                case head :: tail => head(input) match {
                    case ParseResult.Error(_, _) => tryParse(tail)
                    case ParseResult.Success(value, rest) => ParseResult.Success(value, rest)
                }
            }

            tryParse(parsers)
        }

        def parseWord: Parser[StringSlice] = parseAtLeastOnce(_.isLetter)

    }


    sealed trait ParseResult[T] {
        val rest: StringSlice

        def map[A](f: T => A): ParseResult[A] = {
            this match {
                case ParseResult.Success(value, rest) => ParseResult.Success(f(value), rest)
                case ParseResult.Error(value, rest) => ParseResult.Error(value, rest)
            }
        }
        def flatMap[A](f: (T, StringSlice) => ParseResult[A]): ParseResult[A] = {
            this match {
                case ParseResult.Success(value, rest) => f(value, rest)
                case ParseResult.Error(value, rest) => ParseResult.Error(value, rest)
            }
        }

        def andThen[A](f: StringSlice => ParseResult[A]): ParseResult[(T, A)] = {
            this match {
                case ParseResult.Success(value, rest) => f(rest).map(secondValue => (value, secondValue))
                case ParseResult.Error(value, rest) => ParseResult.Error(value, rest)
            }
        }
    }

    object ParseResult {
        final case class Success[T](value: T, rest: StringSlice) extends ParseResult[T]
        final case class Error[T](error: String, rest: StringSlice) extends ParseResult[T]
    }
}
