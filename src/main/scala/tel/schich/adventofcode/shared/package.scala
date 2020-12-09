package tel.schich.adventofcode

import tel.schich.adventofcode.shared.Parser.parseRepeated

import scala.annotation.tailrec

package object shared {

    trait Parser[A] extends (String => ParseResult[A]) { self =>

        def map[B](f: A => B): Parser[B] = (input: String) => apply(input).map(f)

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

        def check(predicate: String => Boolean): Parser[Boolean] =
            (input: String) => ParseResult.Success(predicate(input), input)

        def check(string: String): Parser[Boolean] = check(i => i.startsWith(string))

        def parseString(s: String): Parser[String] = input => {
            if (input.startsWith(s)) ParseResult.Success(s, input.substring(s.length))
            else ParseResult.Error(new Exception(s"Did not match string $s"), input)
        }

        def parseOneOf(s: Seq[Char]): Parser[Char] = parseOne(c => s.contains(c))

        def parseFixedLengthString(n: Int): Parser[String] = input => {
            if (input.length < n) ParseResult.Error(new Exception(s"Not enough input left! Required $n"), input)
            else {
                val (string, rest) = input.splitAt(n)
                ParseResult.Success(string, rest)
            }
        }

        def parseOne(predicate: Char => Boolean): Parser[Char] = input => input.headOption match {
            case Some(c) if predicate(c) => ParseResult.Success(c, input.substring(1))
            case _ => ParseResult.Error(new Exception("nothing found!"), input)
        }

        def parseWhile(predicate: Char => Boolean): Parser[String] = input => {
            val string = input.takeWhile(predicate)
            ParseResult.Success(string, input.substring(string.length))
        }

        def parseAtLeastOnce(predicate: Char => Boolean): Parser[String] = parseOne(predicate).flatMap { first =>
            parseWhile(predicate).map(s => s"$first$s")
        }

        def parseWhitespace: Parser[String] = parseWhile(_.isWhitespace)

        def parseSpaces: Parser[String] = parseAtLeastOnce(c => c.isWhitespace && c != '\r' && c != '\n')

        def parseLineBreak: Parser[String] = parseOneOf("\r\n").flatMap {
            case '\r' => parseString("\n").?.map {
                case Some(_) => "\r\n"
                case None => "\r"
            }
            case '\n' => noop("\n")
        }

        def parseChar: Parser[Char] = input => {
            input.headOption match {
                case Some(c) => ParseResult.Success(c, input.substring(1))
                case _ => ParseResult.Error(new Exception("No more input"), input)
            }
        }

        def parseRepeated[A](n: Int, parser: Parser[A]): Parser[Seq[A]] = input => {
            @tailrec
            def loop(input: String, n: Int, carry: Seq[A]): ParseResult[Seq[A]] = {
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
            def loop(input: String, carry: Seq[A]): ParseResult[Seq[A]] = {
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
                digits.foldLeft((firstDigit - '0').toLong)((num, digit) => num * 10 + (digit - '0'))
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
                case Nil => ParseResult.Error(new Exception("no more parser to try!"), input)
                case head :: tail => head(input) match {
                    case ParseResult.Error(_, _) => tryParse(tail)
                    case ParseResult.Success(value, rest) => ParseResult.Success(value, rest)
                }
            }

            tryParse(parsers)
        }

    }


    sealed trait ParseResult[T] {
        val rest: String

        def map[A](f: T => A): ParseResult[A] = {
            this match {
                case ParseResult.Success(value, rest) => ParseResult.Success(f(value), rest)
                case ParseResult.Error(value, rest) => ParseResult.Error(value, rest)
            }
        }
        def flatMap[A](f: (T, String) => ParseResult[A]): ParseResult[A] = {
            this match {
                case ParseResult.Success(value, rest) => f(value, rest)
                case ParseResult.Error(value, rest) => ParseResult.Error(value, rest)
            }
        }

        def andThen[A](f: String => ParseResult[A]): ParseResult[(T, A)] = {
            this match {
                case ParseResult.Success(value, rest) => f(rest).map(secondValue => (value, secondValue))
                case ParseResult.Error(value, rest) => ParseResult.Error(value, rest)
            }
        }
    }

    object ParseResult {
        final case class Success[T](value: T, rest: String) extends ParseResult[T]
        final case class Error[T](error: Throwable, rest: String) extends ParseResult[T]
    }
}
