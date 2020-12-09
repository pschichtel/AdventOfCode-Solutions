name := "AdventOfCode"
version := "1.0.0"
scalaVersion := "2.13.4"

enablePlugins(ScalaJSPlugin)

scalacOptions ++= Seq("-unchecked", "-deprecation")

sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false

scalaJSUseMainModuleInitializer := true
//scalaJSMainModuleInitializer in Global := Some(ModuleInitializer.mainMethod("tel.schich.adventofcode.year2020.RunAll", "main"))
mainClass in Compile := Some("tel.schich.adventofcode.year2020.RunAll")

sourceGenerators in Compile += Def.task {
    val baseDir = baseDirectory.value / "inputs" // or whatever

    val filesForYears = {
        if (!baseDir.isDirectory) Seq.empty
        else {
            baseDir.listFiles(f => f.isDirectory && f.name.matches("\\d+")).toSeq.map { yearDir =>
                val year = yearDir.name.toInt
                val inputs = yearDir.listFiles(f => !f.isDirectory && f.name.endsWith(".txt")).toSeq.map { input =>
                    IO.read(input).trim
                }

                (year, inputs)
            }
        }
    }


    val targetPackage = Seq("tel", "schich", "adventofcode", "generated")
    val sourceDir = (sourceManaged in Compile).value

    for ((year, inputs) <- filesForYears) yield {
        val sourceFile = sourceDir / s"Inputs$year.scala"
        val fields = inputs
                .zipWithIndex
                .map { case (content, i) =>
                    val fieldName = s"Day${(i + 1).formatted("%02d")}"
                    val stringPrefix = "raw\"\"\""
                    val stringSuffix = "\"\"\""
                    val stringSeparator = s"$stringSuffix + $stringPrefix"
                    val value = content
                            .grouped(50000)
                            .map(_.replaceAllLiterally("$", "$$"))
                            .mkString(stringPrefix, stringSeparator, stringSuffix)
                    s"  final val $fieldName = $value"
                }
                .mkString("", "\n", "\n")


        val scalaCode =
            s"""
               |package ${targetPackage.mkString(".")}
               |
               |final case object Input$year {
               |$fields
               |}
            """.stripMargin

        IO.write(sourceFile, scalaCode)
        sourceFile
    }
}.taskValue