package com.andbutso.ajatella

object Case {
  import Alternate._

  // TODO Implement NounCase.detect
  def detect(word: String) = {
    Seq.empty[Case]
  }

  def apply(letters: Letters): Case = {
    new Case(Set(letters))
  }

  // Grammatical
  case object Nominative extends Case(Set(Letters("")))
  case object Genetive   extends Case(Set(Letters("n")))
  case object Accusative extends Case(Set(Letters("t")))
  case object Partative  extends Case(Set(Letters(A), Letters("t", A), Letters("tt", A)))

  // Locative (internal)
  case object Inessive extends Case(Set(Letters("ss", A)))
  case object Elative  extends Case(Set(Letters("st", A)))
  case object Illative extends Case(Set(Letters(V, "n"), Letters("h", V, "n"), Letters("seen")))

  // Locative (external)
  case object Adessive extends Case(Set(Letters("ll", A)))
  case object Ablative extends Case(Set(Letters("lt", A)))
  case object Allative extends Case(Set(Letters("lle")))

  // Essive
  case object Essive      extends Case(Set(Letters("n", A)))
  case object Translative extends Case(Set(Letters("ksi")))

  // Marginal

  case object Comitative extends Case(Set(Letters("ine")))
  case object Abessive   extends Case(Set(Letters("tt", A)))
  // N.B. Singular form extremely rare
  case object Instructive extends Case(Set(Letters("n")))

  // Frequency distribution here: https://www.cs.tut.fi/~jkorpela/finnish-cases.html
  val All = Set[Case](
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

class Case(val suffixes: Set[Letters]) extends Suffix {
  def letters = null // TODO Choose the right suffix
}




