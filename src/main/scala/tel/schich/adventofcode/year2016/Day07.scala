package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day07 extends AoCApp {

    val input = asLines(Input2016.Day07)
    val ABBA = "(\\w)(\\w)\\2\\1".r
    val ABA = "(\\w)(\\w)\\1".r

    def checkForABBA(s: String): Boolean = {
        ABBA.findFirstIn(s) match {
            case Some(abba) => abba(0) != abba(1)
            case None => false
        }
    }

    def findABAs(s: String): Seq[String] = {
        s.toSeq.sliding(3).filter(s => s(0) == s(2) && s(0) != s(1)).map(_.unwrap).toSeq
    }

    def supernetSeq(i: Int): Boolean = i % 2 == 0
    def hypernetSeq(i: Int): Boolean = i % 2 == 1

    def splitSuperHyper(s: String): (Seq[String], Seq[String]) = {
        val withIndex = s.split("[\\[\\]]").toVector.zipWithIndex
        val superSeqs = withIndex.filter(p => supernetSeq(p._2)).map(_._1)
        val hyperSeqs = withIndex.filter(p => hypernetSeq(p._2)).map(_._1)
        (superSeqs, hyperSeqs)
    }

    def supportsTLS(ip: String): Boolean = {
        val (superSeqs, hyperSeqs) = splitSuperHyper(ip)
        superSeqs.exists(checkForABBA) && !hyperSeqs.exists(checkForABBA)
    }

    def supportsSSL(ip: String): Boolean = {
        def abaToBab(aba: String) = "" + aba(1) + aba(0) + aba(1)

        val (superSeqs, hyperSeqs) = splitSuperHyper(ip)
        superSeqs.exists {s =>
            findABAs(s).map(abaToBab).exists(bab => hyperSeqs.exists(_.contains(bab)))
        }
    }

    part(1, input.count(supportsTLS))

    part(2, input.count(supportsSSL))
}
