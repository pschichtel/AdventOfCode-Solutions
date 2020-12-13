name := "AdventOfCode"
version := "1.0.0"
scalaVersion := "2.13.4"

// enablePlugins(ScalaJSPlugin)
// enablePlugins(ScalaNativePlugin)

scalacOptions ++= Seq("-unchecked", "-deprecation")

sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false

libraryDependencies ++= Seq("org.jblas" % "jblas" % "1.2.5")

// scalaJSUseMainModuleInitializer := true
// mainClass in Compile := Some("tel.schich.adventofcode.year2020.RunAll")

// nativeMode := "release"
// nativeGC := "immix"
// nativeLTO := "thin"

sourceGenerators in Compile += Def.task {
    val baseDir = baseDirectory.value / "inputs" // or whatever

    val filesForYears = {
        if (!baseDir.isDirectory) Seq.empty
        else {
            baseDir.listFiles(f => f.isDirectory && f.name.matches("\\d+")).toSeq.map { yearDir =>
                val year = yearDir.name.toInt
                val inputs = yearDir.listFiles(f => !f.isDirectory && f.name.endsWith(".txt")).toSeq.map { input =>
                    (input.name.substring(0, input.name.length - 4), IO.read(input).trim)
                }

                (year, inputs.sortBy(_._1))
            }
        }
    }


    val sourceDir = (sourceManaged in Compile).value

    for ((year, inputs) <- filesForYears) yield {
        val sourceFile = sourceDir / s"Inputs$year.scala"
        val fields = inputs
                .map { case (name, content) =>
                    val stringPrefix = "raw\"\"\""
                    val stringSuffix = "\"\"\""
                    val stringSeparator = s"$stringSuffix + $stringPrefix"
                    val value = content
                            .grouped(50000)
                            .map(_.replaceAllLiterally("$", "$$"))
                            .mkString(stringPrefix, stringSeparator, stringSuffix)
                    s"  final val $name = $value"
                }
                .mkString("", "\n", "\n")


        val targetPackage = Seq("tel", "schich", "adventofcode", s"year$year")
        val scalaCode =
            s"""
               |package ${targetPackage.mkString(".")}
               |
               |private[year$year] final case object Input$year {
               |$fields
               |}
            """.stripMargin

        IO.write(sourceFile, scalaCode)
        sourceFile
    }
}.taskValue