package tel.schich.adventofcode.year2016

import tel.schich.adventofcode.shared.AoCApp

object Day08 extends AoCApp {

    type Screen = Vector[Boolean]
    type ScreenOperation = (Screen, Int, Int) => Screen

    def coord2index(x: Int, y: Int, width: Int): Int = y * width + x
    def index2coord(i: Int, width: Int): (Int, Int) = {
        val y = math.floor(i / width).toInt
        (i - y * width, y)
    }

    def mapOverScreen(screen: Screen, width: Int, height: Int)(f: (Screen, Int, Int, Boolean) => Boolean): Screen = {
        screen.zipWithIndex.map {
            case (state, i) =>
                val (x, y) = index2coord(i, width)
                f(screen, x, y, state)
        }
    }

    def enableRect(a: Int, b: Int)(screen: Screen, width: Int, height: Int): Screen = {
        mapOverScreen(screen, width, height) { (_, x, y, state) =>
            if (x < a && y < b) true
            else state
        }
    }

    def rotateRow(row: Int, b: Int)(screen: Screen, width: Int, height: Int): Screen = {
        mapOverScreen(screen, width, height) { (screen, x, y, state) =>
            if (y == row) {
                screen(coord2index((x - b + width) % width, y, width))
            } else state
        }
    }

    def rotateColumn(column: Int, b: Int)(screen: Screen, width: Int, height: Int): Screen = {
        mapOverScreen(screen, width, height) { (screen, x, y, state) =>
            if (x == column) {
                screen(coord2index(x, (y - b + height) % height,  width))
            } else state
        }
    }

    def simulateScreen(width: Int, height: Int, ops: Seq[ScreenOperation]): Screen = {
        val screen = Vector.fill(width * height)(false)

        ops.foldLeft(screen)((screen, op) => op(screen, width, height))
    }

    def printScreen(screen: Screen, width: Int, height: Int): Unit = {
        println("=" * width)
        for (y <- 0 until height) {
            for (x <- 0 until width) {
                print(if (screen(coord2index(x, y, width))) '#' else '_')
            }
            println()
        }
        println("=" * width)
    }

    def subScreen(screen: Screen, width: Int, xOffset: Int, yOffset: Int, outWidth: Int, outHeight: Int): Screen = {
        val outScreen = for {
            y <- 0 until outHeight
            x <- 0 until outWidth
        } yield {
            screen(coord2index(xOffset + x, yOffset + y, width))
        }
        outScreen.toVector
    }

    def ocr(screen: Screen, screenWidth: Int, charWidth: Int, charHeight: Int, charMap: Map[Screen, Char], unknownChar: Char = '?'): String = {
        (0 until screenWidth / charWidth).map { i =>
            val sub = subScreen(screen, screenWidth, charWidth * i, 0, charWidth, charHeight)
            charMap.getOrElse(sub, unknownChar)
        }.mkString
    }

    override def solution: (Any, Any) = {
        val width = 50
        val height = 6

        val rect = "rect (\\d+)x(\\d+)".r
        val rotRow = "rotate row y=(\\d+) by (\\d+)".r
        val rotCol = "rotate column x=(\\d+) by (\\d+)".r

        val operations = asLines(Input2016.Day08).map {
            case rect(a, b) => enableRect(a.toInt, b.toInt) _
            case rotRow(y, b) => rotateRow(y.toInt, b.toInt) _
            case rotCol(x, b) => rotateColumn(x.toInt, b.toInt) _
        }

        val finalScreen = simulateScreen(width, height, operations)
        val part1 = finalScreen.count(identity)


        def c(s: String) = s.map {
            case '_' => false
            case '#' => true
        }.toVector

        val charMap = Map(
            c("_##__" + "#__#_" + "#__#_" + "####_" + "#__#_" + "#__#_") -> 'A',
            c("###__" + "#__#_" + "###__" + "#__#_" + "#__#_" + "###__") -> 'B',
            c("####_" + "#____" + "###__" + "#____" + "#____" + "#____") -> 'F',
            c("__##_" + "___#_" + "___#_" + "___#_" + "#__#_" + "_##__") -> 'J',
            c("###__" + "#__#_" + "#__#_" + "###__" + "#____" + "#____") -> 'P',
            c("_###_" + "#____" + "#____" + "_##__" + "___#_" + "###__") -> 'S',
            c("#__#_" + "#__#_" + "#__#_" + "#__#_" + "#__#_" + "_##__") -> 'U',
            c("####_" + "___#_" + "__#__" + "_#___" + "#____" + "####_") -> 'Z'
        )

        val part2 = ocr(finalScreen, width, 5, 6, charMap)

        (part1, part2)
    }
}
