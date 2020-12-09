package tel.schich.adventofcode.year2020

import tel.schich.adventofcode.shared.AoCApp

import java.util.concurrent.TimeUnit

object RunAll {

    val apps: Seq[AoCApp] = Seq(
        Day01,
        Day02,
        Day03,
        Day04,
        Day05,
        Day06,
        Day07,
        Day08,
        Day09,
    )

    def main(args: Array[String]): Unit = {
        val appArgs = Array("silent")
        // warmup
        //apps.foreach(_.main(appArgs))

        val overallStart = System.nanoTime()
        apps.foreach { app =>
            println(s"App: ${app.name}")
            val appStart = System.nanoTime()
            app.main(appArgs)
            val appTime = TimeUnit.MICROSECONDS.convert(System.nanoTime() - appStart, TimeUnit.NANOSECONDS)
            println(s"Time: $appTime µs")
        }
        val overallTime = TimeUnit.MICROSECONDS.convert(System.nanoTime() - overallStart, TimeUnit.NANOSECONDS)
        println(s"Overall: $overallTime µs")
    }

}
