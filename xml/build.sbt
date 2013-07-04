organization := "com.jteigen.macaroon"

name := "xml"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.2"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)

libraryDependencies += "no.arktekk" %% "anti-xml" % "0.5.1"