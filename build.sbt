name := "ajatella"

version := "1.0"

scalaVersion := "2.11.7"

dependencyOverrides += "org.scala-lang" % "scala-compiler" % scalaVersion.value

libraryDependencies ++= Seq(
	"org.json4s" %% "json4s-native" % "3.2.11",
	"org.json4s" %% "json4s-jackson" % "3.2.11",
  "com.googlecode.kiama" %% "kiama" % "1.8.0",
  "org.specs2" %% "specs2-core" % "3.6.2" % "test",
  "org.specs2" %% "specs2-mock" % "3.6.2" % "test",
  "org.specs2" %% "specs2-junit" % "3.6.2" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

initialCommands := """
import com.andbutso.ajatella._
import Conversions._
import Case._
import Lexeme._
import Grapheme._
import Vowel._
import Alternate._ 
import Consonants._
import Vowels._
import Alternate.Identity._
import GraphemeMatcher._
"""
