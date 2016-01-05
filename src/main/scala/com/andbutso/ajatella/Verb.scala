package com.andbutso.ajatella

object Verb {
  // TODO DerivationSuffixes, starting on Karlsson page 278
  object DerivationSuffix {

  }
}

trait Verb
trait NonFiniteVerb extends Verb
trait Participle extends NonFiniteVerb

case class ActiveParticiple(letters: Letters) extends Suffix
object ActiveParticiple {
  import Alternate._

  // Present
  val VA  = ActiveParticiple(Letters("v", A))
  // Past
  val NUT = ActiveParticiple(Letters("n", U, "t"))
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

abstract class Mood(val letters: Letters) extends Suffix
object Mood {
  case object Indicative extends Mood(Letters(""))
  case object Conditional extends Mood(Letters("isi"))
  case object Potential extends Mood(Letters("ne")) // TODO And others
  case object Imperative extends Mood(Letters("")) // TODO Tons of exceptions based on person
}

trait Tense

object Tense {
  case object Infinitive extends Tense
  case object Present extends Tense
  case object Imperfect extends Tense
  case object Perfect extends Tense
  case object Pluperfect extends Tense
}

trait Person
object Person {
  case object First  extends Person
  case object Second extends Person
  case object Third  extends Person
}

trait Pronoun // minä, sinä, hän, me, te, he