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
    import Consonants._
    import Vowels._
    import Alternate.Identity._

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
    case object hkO extends DerivationSuffix(h∙k∙O)

    // -inen (adjective)
    //
    // hiki - sweat : hikinen  - sweaty
    // jää  - ice   : jäinen   - icy
    // lika - dirt  : likainen - dirty
    case object inen extends DerivationSuffix(i∙n∙e∙n)

    // -isA (adjective)
    //
    // kala   - fish : kalaisa  - abounding in fish
    // leikki - play : leikkisä - playful
    // raivo  - fury : raivoisa - furious
    case object isA extends DerivationSuffix(Vowels.i∙s∙A)

    /*
    * -kkO (collective noun)
    *
    * aalto  - wave   : aallokko  - the waves, swell
    * kuusi  - spruce : kuusikko  - spruce grove
    * pensas - bush   : pensaikko - thicket, shrubbery
    */
    case object kkO extends DerivationSuffix(k∙k∙O)

    /*
    * -lA (noun, indicates location)
    *
    * kahvi   - coffee : kahvila   - café
    * ravinto - food   : ravintola - restaurant
    * sairas  - ill    : sairaala  - hospital
    */
    case object lA extends DerivationSuffix(l∙A)

    /*
    * -lAinen (noun, or noun and adjective, indicates a person)
    *
    * apu   - help    : apulainen   - assistant
    * pako  - flight  : pakolainen  - refugee
    * työ   - work    : työläinen   - worker
    * Suomi - Finland : suomalainen - Finn, Finnish
    */
    case object lAinen extends DerivationSuffix(l∙A∙i∙n∙e∙n)

    // TODO The adjectival -lAinen which can't have the same name as the noun lAinen case object

    /*
    * -llinen (adjective)
    */
    case object llinen extends DerivationSuffix(l∙l∙i∙n∙e∙n)

    /*
    * -mAinen (adjective)
    */
    case object mAinen extends DerivationSuffix(m∙A∙i∙n∙e∙n)

    /*
    * -nAinen (adjective)
    */
    case object nAinen extends DerivationSuffix(n∙A∙i∙n∙e∙n)

    /*
    * -nen (diminutive noun)
    */
    case object nen extends DerivationSuffix(n∙e∙n)

    /*
    * -stO (collective noun)
    */
    case object stO extends DerivationSuffix(s∙t∙O)

    /*
    * -tAr (feminine noun)
    */
    case object tAr extends DerivationSuffix(t∙A∙r)

    /*
    * -tOn (adjective, indicating 'without')
    */
    case object tOn extends DerivationSuffix(t∙O∙n)

    /*
    * -(U)Us (abstract noun)
    */
    case object UUs extends DerivationSuffix(U∙U∙s)
    case object Us  extends DerivationSuffix(U∙s)

    /***********************
     * Nominals from Verbs *
     **********************/
    // TODO Deal with namespace clash caused by importing Alternate._
//    /*
//    * -e (noun)
//    */
//    case object e extends DerivationSuffix(Letters("e"))
//
//    /*
//    * -i (noun)
//    */
//    case object i extends DerivationSuffix(Vowels.i)

    /*
    * -in (noun, indicate instrument)
    */
    case object in extends DerivationSuffix(i∙n)

    /*
    * -jA (noun, indicates agent)
    */
    case object jA extends DerivationSuffix(j∙A)

    /*
    * -mAtOn (negative adjective)
    */
    case object mAtOn extends DerivationSuffix(m∙A∙t∙O∙n)

    /*
    * -ntA (noun)
    */
    case object ntA extends DerivationSuffix(n∙t∙A)

    /*
    * -nti (noun)
    */
    case object nti extends DerivationSuffix(n∙t∙i)

    /*
    * -ntO (noun)
    */
    case object ntO extends DerivationSuffix(n∙t∙O)

    /*
    * -minen (deverbal noun)
    */
    case object minen extends DerivationSuffix(m∙i∙n∙e∙n)

    /*
    * -O (noun)
    */
    // TODO Deal with namespace clash caused by importing Alternate._
    //  case object O extends NominalDerivation(Letters(O))

    /*
    * -Os (noun, often indicates result of an action)
    */
    case object Os extends DerivationSuffix(O∙s)

    /*
    * -ri (noun, indicates agent)
    */
    case object ri extends DerivationSuffix(r∙i)

    /*
    * -U (noun)
    */
    // TODO Deal with namespace clash caused by importing Alternate._
    //  case object U extends NominalDerivation(Letters(U))

    /*
    * -Us (noun)
    */
//    case object Us extends DerivationSuffix(Letters(U, "s"))

    /*
    * -UU (noun)
    */
    case object UU extends DerivationSuffix(U∙U)

    /*
    * -vAinen (adjective)
    */
    case object vAinen extends DerivationSuffix(v∙A∙i∙n∙e∙n)
  }
}

case class Root(letters: Letters)


