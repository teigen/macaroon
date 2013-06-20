organization := "org.scalamacros"

version := "1.0-SNAPSHOT"

// scalacOptions ++= Seq("-Ymacro-debug-lite")

scalaVersion := "2.11.0-SNAPSHOT"

scalaOrganization := "org.scala-lang.macro-paradise"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies <+= (scalaVersion)("org.scala-lang.macro-paradise" % "scala-reflect" % _)