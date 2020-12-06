package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser
import tel.schich.adventofcode.shared.Parser._

import scala.util.matching.Regex

object Day04 extends AoCApp{

    case class Passport(fields: Map[String, String])

    val parseName = parseAtLeastOnce(c => !c.isWhitespace && c != ':')
    val parseValue = parseWhile(!_.isWhitespace)

    val parsePair = for {
        name <- parseName
        _ <- parseString(":")
        value <- parseValue
    } yield (name, value)

    val parsePassportLine = parseAllSeparated(parsePair, parseSpaces)

    val parsePassport = parseAllSeparated(parsePassportLine, parseLineBreak)
        .map(pairs => Passport(pairs.flatten.toMap))

    implicit val parseInput: Parser[Seq[Passport]] = parseAllSeparated(parsePassport, parseLineBreak.repeated(2))
        .andThenIgnore(parseWhitespace)

    val passports = input[Seq[Passport]]

    def hasRequiredField(p: Passport): Boolean =
        p.fields.size == 8 || p.fields.size == 7 && !p.fields.contains("cid")

    part(1, passports.count(hasRequiredField))

    val fieldValidationRules = Seq(
        ("byr", "(?:19[2-9]\\d|200[012])".r),
        ("iyr", "20(?:1\\d|20)".r),
        ("eyr", "20(?:2\\d|30)".r),
        ("hgt", "(?:1(?:[5-8]\\d|9[0-3])cm|(?:59|6\\d|7[0-6])in)".r),
        ("hcl", "#[\\da-f]{6}".r),
        ("ecl", "(?:amb|blu|brn|gry|grn|hzl|oth)".r),
        ("pid", "\\d{9}".r),
        ("cid", ".+".r),
    )

    def allFieldsValid(passport: Passport, rules: Seq[(String, Regex)]): Boolean = rules.forall {
        case (field, pattern) => passport.fields.get(field).forall(pattern.matches(_))
    }

    passports.filter(p => hasRequiredField(p) && allFieldsValid(p, fieldValidationRules))
        .foreach(println)

    part(2, passports.count(p => hasRequiredField(p) && allFieldsValid(p, fieldValidationRules)))
}
