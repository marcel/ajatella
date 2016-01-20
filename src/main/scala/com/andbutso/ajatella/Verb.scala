package com.andbutso.ajatella

object Verb {
  // TODO DerivationSuffixes, starting on Karlsson page 278
  object DerivationSuffix {

  }
}

trait Verb
trait NonFiniteVerb extends Verb
trait Participle extends NonFiniteVerb

case class ActiveParticiple(ending: GraphemeMatcher) extends Suffix
object ActiveParticiple {
  import Alternate._
  import Consonants._

  // Present
  val VA  = ActiveParticiple(v∙A)
  // Past
  val NUT = ActiveParticiple(n∙U∙t)
}

case class Infinitive(suffixes: Set[Letters])

object Infinitive {
  import com.andbutso.ajatella.{Alternate => Alt}

  val A  = Infinitive(Set(Letters(Alt.A), Letters("d", Alt.A), Letters("t", Alt.A)))
  val E  = Infinitive(Set(Letters("e"), Letters("de"), Letters("te")))
  val MA = Infinitive(Set(Letters("m", Alt.A)))
}

case class FiniteVerb(
  root: Root,
  isPassive: Boolean,
  tenseOrMood: Either[Tense, Mood],
  person: Person,
  clitic: Option[Clitic]
) extends Verb

abstract class Mood(val ending: GraphemeMatcher) extends Suffix
object Mood {
  import Alternate.Blank
  import Consonants._
  import Vowels._

  case object Indicative extends Mood(Blank)
  case object Conditional extends Mood(i∙s∙i)
  case object Potential extends Mood(n∙e) // TODO And others
  case object Imperative extends Mood(Blank) // TODO Tons of exceptions based on person
}

trait Tense

object Tense {
  case object Infinitive extends Tense
  case object Present extends Tense
  case object Imperfect extends Tense
  case object Perfect extends Tense
  case object Pluperfect extends Tense
}

trait TextualDescription {
  def toText: String
}

trait Person extends TextualDescription {
  import Number._

  def singular(ending: GraphemeMatcher) = Possessive(this, Singular, ending)
  def plural(ending: GraphemeMatcher)   = Possessive(this, Plural, ending)
}

object Person {
  case object First  extends Person {
    def toText = "1st person"
  }
  case object Second extends Person {
    def toText = "2nd person"
  }
  case object Third  extends Person {
    def toText = "3rd person"
  }

  val FirstPerson  = First
  val SecondPerson = Second
  val ThirdPerson  = Third
}

trait Pronoun // minä, sinä, hän, me, te, he