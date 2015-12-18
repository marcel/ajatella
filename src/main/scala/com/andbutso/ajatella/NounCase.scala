package com.andbutso.ajatella

import scala.collection.immutable.SortedSet


object NounCase {
  import Alternate._

  // TODO Implement NounCase.detect
  def detect(word: String) = {
    Seq.empty[NounCase]
  }

  def apply(letters: Letters): NounCase = {
    new NounCase(Set(letters))
  }

  // Grammatical
  case object Nominative extends NounCase(Set(Letters("")))
  case object Genetive   extends NounCase(Set(Letters("n")))
  case object Accusative extends NounCase(Set(Letters("t")))
  case object Partative  extends NounCase(Set(Letters(A), Letters("t", A), Letters("tt", A)))

  // Locative (internal)
  case object Inessive extends NounCase(Set(Letters("ss", A)))
  case object Elative  extends NounCase(Set(Letters("st", A)))
  case object Illative extends NounCase(Set(Letters(V, "n"), Letters("h", V, "n"), Letters("seen")))

  // Locative (external)
  case object Adessive extends NounCase(Set(Letters("ll", A)))
  case object Ablative extends NounCase(Set(Letters("lt", A)))
  case object Allative extends NounCase(Set(Letters("lle")))

  // Essive
  case object Essive      extends NounCase(Set(Letters("n", A)))
  case object Translative extends NounCase(Set(Letters("ksi")))

  // Marginal

  case object Comitative extends NounCase(Set(Letters("ine")))
  case object Abessive   extends NounCase(Set(Letters("tt", A)))
  // N.B. Singular form extremely rare
  case object Instructive extends NounCase(Set(Letters("n")))

  // Frequency distribution here: https://www.cs.tut.fi/~jkorpela/finnish-cases.html
  val All = Set[NounCase](
    // Grammatical
    Nominative,
    Genetive,
    Accusative,
    Partative,
    // Locative (internal)
    Inessive,
    Elative,
    Illative,
    // Locative (external)
    Adessive,
    Ablative,
    Allative,
    // Essive
    Essive,
    Translative,
    // Marginal
    Instructive,
    Abessive,
    Comitative
  )
}

class NounCase(val suffixes: Set[Letters]) extends Suffix {
  def letters = null // TODO Choose the right suffix
}




