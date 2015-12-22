import scala.io.Source
import scala.math._

println("Day  5")

val input = Source.fromFile("day5.txt").mkString.trim.split('\n').map(_.trim).toSeq

val vowels = Set('a', 'e', 'i', 'o', 'u')
val forbiddenWindows = Set("ab", "cd", "pq", "xy")

val nice = input.filter {s =>
    val vowelCount = s.filter(vowels.contains(_)).length
    val windows = s.sliding(2).toList

    val doubleWindowCount = windows.filter(w => w(0) == w(1)).length
    val forbiddenWindowCount = windows.filter(forbiddenWindows.contains(_)).length

    vowelCount >= 3 && doubleWindowCount >= 1 && forbiddenWindowCount == 0
}

println(s"Part 1: ${nice.length}")

def sliding(s: String, n: Int = 2) =
    s.zipWithIndex.map {case (c, i) => (i, s.substring(i, min(s.length, i + n)))}.filter {case (_, s) => s.length == n}

val nicer = input.filter {s =>
    val reoccuringSlides = sliding(s).collect {
        case (i, w) if s.substring(0, i).contains(w) || s.substring(i + 2).contains(w) => Some(w)
    }

    var triple = s.sliding(3).filter(w => w(0) == w(2)).toList.size
    reoccuringSlides.length >= 1 && triple >= 1
}

println(s"Part 2: ${nicer.length}")

