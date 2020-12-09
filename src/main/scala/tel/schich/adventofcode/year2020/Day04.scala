package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp
import tel.schich.adventofcode.shared.Parser._
import tel.schich.adventofcode.shared.StringSlice

import scala.util.matching.Regex

object Day04 extends AoCApp {

    val CountryId = StringSlice("cid")
    case class Passport(fields: Map[StringSlice, String])

    val parseName = parseWord
    val parseValue = parseWhile(!_.isWhitespace)

    val parsePair = for {
        name <- parseName
        _ <- parseString(":")
        value <- parseValue.map(_.asString)
    } yield (name, value)

    val parsePassportLine = parseAllSeparated(parsePair, parseSpaces)

    val parsePassport = parseAllSeparated(parsePassportLine, parseLineBreak)
        .map(pairs => Passport(pairs.flatten.toMap))

    val parseInput = parseAllSeparated(parsePassport, parseLineBreak.repeated(2))
        .andThenIgnore(parseWhitespace)

    val passports = parse(Input2020.Day04, parseInput).toList

    def hasRequiredField(p: Passport): Boolean =
        p.fields.size == 8 || p.fields.size == 7 && !p.fields.contains(CountryId)

    val passportsWithRequiredFields = passports.filter(hasRequiredField)
        part(1, passportsWithRequiredFields.length)

    val fieldValidationRules = Seq(
        (StringSlice("byr"), "(?:19[2-9]\\d|200[012])".r),
        (StringSlice("iyr"), "20(?:1\\d|20)".r),
        (StringSlice("eyr"), "20(?:2\\d|30)".r),
        (StringSlice("hgt"), "(?:1(?:[5-8]\\d|9[0-3])cm|(?:59|6\\d|7[0-6])in)".r),
        (StringSlice("hcl"), "#[\\da-f]{6}".r),
        (StringSlice("ecl"), "(?:amb|blu|brn|gry|grn|hzl|oth)".r),
        (StringSlice("pid"), "\\d{9}".r),
    )

    def allFieldsValid(passport: Passport, rules: Seq[(StringSlice, Regex)]): Boolean = rules.forall {
        case (field, pattern) => passport.fields.get(field).forall(pattern.matches)
    }

    part(2, passportsWithRequiredFields.count(p => allFieldsValid(p, fieldValidationRules)))
}
