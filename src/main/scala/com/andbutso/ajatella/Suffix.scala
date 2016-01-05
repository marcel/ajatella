package com.andbutso.ajatella

object Suffix {
  val All = {
    Case.All ++ Nominal.DerivationSuffix.All
  }
}

// TODO If/when suffix is molded into the right generalization then probably the
// :+: method on Case should be moved up into Suffix so that appending any type of suffix
// onto a lexeme performs consonant gradation if necessary and selects
// the appropriate front or back vowel if applicable
trait Suffix {
  def letters: Letters
}

class DerivationSuffix(val letters: Letters) extends Suffix