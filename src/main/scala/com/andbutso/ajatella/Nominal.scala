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
  nounCase: Case,
  possessive: Option[Possessive] = None,
  clitic: Option[Clitic] = None
) {
  def number(newNumber: Number): Nominal = {
    copy(number = newNumber)
  }

  def decline(newCase: Case) = {
    copy(nounCase = newCase)
  }

  def possessive(newPossessive: Possessive) = {
    copy(possessive = Some(newPossessive))
  }
}

object Nominal {
  implicit def stringToNominal(string: String): Nominal = {
    require(string.indexOf(" ") == -1)

    Nominal(
      Root(Letters(string.toCharArray)),
      Number.Singular,
      Case.Nominative
    )
  }
}

case class Root(letters: Letters)

trait Possessive {
  def value: String
  def number: Number
}

object Possessive {
  import Number._

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



