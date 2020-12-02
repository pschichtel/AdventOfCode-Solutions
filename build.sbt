name := "AdventOfCode"
version := "1.0.0"
scalaVersion := "2.13.4"

scalacOptions ++= Seq("-unchecked", "-deprecation")

sources in (Compile, doc) := Seq.empty

publishArtifact in (Compile, packageDoc) := false
