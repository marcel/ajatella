package com.andbutso.ajatella

// Nominals are:
//
//    1. nouns
//    2. adjectives
//    3. pronouns
//    4. numerals
case class Nominal(
  root: Root,
  number: Number,
  nounCase: NounCase,
  possessive: Option[Possessive],
  clitic: Option[Clitic]
)

case class Root(letters: Letters)

trait Possessive {
  def value: String
  def number: Number
}

object Possessive {
  case class FirstPerson(number: Number) extends Possessive {
    def value = {
      number match {
        case Singular => "ni"
        case Plural   => "mme"
      }
    }
  }

  case class SecondPerson(number: Number) extends Possessive {
    def value = {
      number match {
        case Singular => "si"
        case Plural   => "nne"
      }
    }
  }

  case class ThirdPerson(number: Number) extends Possessive {
    val value = "nsa"
  }
}

trait Suffix {
  def letters: Letters
}



