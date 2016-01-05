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

  object DerivationSuffix {
    import Alternate._

    val All = Seq( // TODO Add remainder
      hkO
    )

    /**************************
     * Nominals from Nominals *
     **************************/

    /*
    * -hkO (adjective, indicates "somewhat")
    *
    * kylmä   - cold : kylmähkö  - rather cold
    * kova    - hard : kovahko   - fairly hard
    * iloinen - glad : iloisehko - fairly glad
    */
    case object hkO extends DerivationSuffix(Letters("hk", O))

    // -inen (adjective)
    //
    // hiki - sweat : hikinen  - sweaty
    // jää  - ice   : jäinen   - icy
    // lika - dirt  : likainen - dirty
    case object inen extends DerivationSuffix(Letters("inen"))

    // -isA (adjective)
    //
    // kala   - fish : kalaisa  - abounding in fish
    // leikki - play : leikkisä - playful
    // raivo  - fury : raivoisa - furious
    case object isA extends DerivationSuffix(Letters("is", A))

    /*
    * -kkO (collective noun)
    *
    * aalto  - wave   : aallokko  - the waves, swell
    * kuusi  - spruce : kuusikko  - spruce grove
    * pensas - bush   : pensaikko - thicket, shrubbery
    */
    case object kkO extends DerivationSuffix(Letters("kk", O))

    /*
    * -lA (noun, indicates location)
    *
    * kahvi   - coffee : kahvila   - café
    * ravinto - food   : ravintola - restaurant
    * sairas  - ill    : sairaala  - hospital
    */
    case object lA extends DerivationSuffix(Letters("l", A))

    /*
    * -lAinen (noun, or noun and adjective, indicates a person)
    *
    * apu   - help    : apulainen   - assistant
    * pako  - flight  : pakolainen  - refugee
    * työ   - work    : työläinen   - worker
    * Suomi - Finland : suomalainen - Finn, Finnish
    */
    case object lAinen extends DerivationSuffix(Letters("l", A, "inen"))

    // TODO The adjectival -lAinen which can't have the same name as the noun lAinen case object

    /*
    * -llinen (adjective)
    */
    case object llinen extends DerivationSuffix(Letters("llinen"))

    /*
    * -mAinen (adjective)
    */
    case object mAinen extends DerivationSuffix(Letters("m", A, "inen"))

    /*
    * -nAinen (adjective)
    */
    case object nAinen extends DerivationSuffix(Letters("n", A, "inen"))

    /*
    * -nen (diminutive noun)
    */
    case object nen extends DerivationSuffix(Letters("nen"))

    /*
    * -stO (collective noun)
    */
    case object stO extends DerivationSuffix(Letters("st", O))

    /*
    * -tAr (feminine noun)
    */
    case object tAr extends DerivationSuffix(Letters("t", A, "r"))

    /*
    * -tOn (adjective, indicating 'without')
    */
    case object tOn extends DerivationSuffix(Letters("t", O, "n"))

    /*
    * -(U)Us (abstract noun)
    */
    case object UUs extends DerivationSuffix(Letters(U, U, "s"))

    /***********************
     * Nominals from Verbs *
     **********************/

    /*
    * -e (noun)
    */
    case object e extends DerivationSuffix(Letters("e"))

    /*
    * -i (noun)
    */
    case object i extends DerivationSuffix(Letters("i"))

    /*
    * -in (noun, indicate instrument)
    */
    case object in extends DerivationSuffix(Letters("in"))

    /*
    * -jA (noun, indicates agent)
    */
    case object jA extends DerivationSuffix(Letters("j", A))

    /*
    * -mAtOn (negative adjective)
    */
    case object mAtOn extends DerivationSuffix(Letters("m", A, "t", O, "n"))

    /*
    * -ntA (noun)
    */
    case object ntA extends DerivationSuffix(Letters("nt", A))

    /*
    * -nti (noun)
    */
    case object nti extends DerivationSuffix(Letters("nti"))

    /*
    * -ntO (noun)
    */
    case object ntO extends DerivationSuffix(Letters("nt", O))

    /*
    * -minen (deverbal noun)
    */
    case object minen extends DerivationSuffix(Letters("minen"))

    /*
    * -O (noun)
    */
    // TODO Deal with namespace clash caused by importing Alternate._
    //  case object O extends NominalDerivation(Letters(O))

    /*
    * -Os (noun, often indicates result of an action)
    */
    case object Os extends DerivationSuffix(Letters(O, "s"))

    /*
    * -ri (noun, indicates agent)
    */
    case object ri extends DerivationSuffix(Letters("ri"))

    /*
    * -U (noun)
    */
    // TODO Deal with namespace clash caused by importing Alternate._
    //  case object U extends NominalDerivation(Letters(U))

    /*
    * -Us (noun)
    */
    case object Us extends DerivationSuffix(Letters(U, "s"))

    /*
    * -UU (noun)
    */
    case object UU extends DerivationSuffix(Letters(U, U))

    /*
    * -vAinen (adjective)
    */
    case object vAinen extends DerivationSuffix(Letters("v", A, "inen"))
  }
}

case class Root(letters: Letters)

trait Possessive {
//  def value: String
//  def number: Number
}

// TODO Implement all possible possessive suffixes based on table on page 296 of Karlsson
object Possessive {
  case object One extends Possessive
  case object Two extends Possessive
//  case class FirstPerson(number: Number) extends Possessive {
//    def value = {
//      number match {
//        case Number.Singular => "ni"
//        case Number.Plural   => "mme"
//      }
//    }
//  }
//
//  case class SecondPerson(number: Number) extends Possessive {
//    def value = {
//      number match {
//        case Singular => "si"
//        case Plural   => "nne"
//      }
//    }
//  }
//
//  case class ThirdPerson(number: Number) extends Possessive {
//    val value = "nsa"
//  }
}


