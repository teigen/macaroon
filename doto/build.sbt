organization := "com.jteigen.macaroon"

name := "doto"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.2"

libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)