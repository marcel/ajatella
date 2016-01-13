package com.andbutso.ajatella

object Morphology {
  import Alternate._

  trait Tag

//  val PossibleStructures = Map(
//    // talosta(an), tytöstä, maasta, kaikesta, kädestä, perheestä, päätöksestä, tehtaasta, totuudesta
//    206 -> Morphology("N", V, None, None, None, Some(Left(Case.Elative.stA)), Some(Possessive.Two))
//  )
}

// TODO Set correct types for each attribute
// TODO Determine if it makes more sense to create subclasses
// for the 3 possible parts of speech
case class Morphology(
  partOfSpeech: String, // N, V or A
  stemEnding: Alternate, // V/C, V, VV, etc
  voice: Option[String], // Active or Passive or N/A
  verbForm: Option[String], // Participle or Infinitive or N/A
  tenseOrMoodOrNumber: Option[String], // how to model 3 possible values? EitherOr :-o
  caseOrPerson: Option[Either[GraphemeMatcher, Person]], //
  possessive: Option[Possessive]
) {
  def matches(lexeme: Lexeme) = {
    // TODO Check for clitics
    // TODO Check for plurals

//    caseOrPerson flatMap {
//      case Left(suffix) if lexeme.endsWith(suffix) =>
//        lexeme.drop(suffix(lexeme))
//      case _ => None
//    } map { truncated =>
//      stemEnding == truncated.letters.last
//    } getOrElse(false)

  }
}

// Steps:
// 1. Check whether there is a clitic. If so, detach it.
// 2. Check whether there is a possessive. If there appears to be one detach it and
// check the remainder in the case/person column.
class MorphologyDetector
