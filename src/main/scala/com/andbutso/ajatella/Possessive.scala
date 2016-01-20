package com.andbutso.ajatella

object Possessive {
  import Consonants._
  import Vowels._
  import Person._
  import Alternate._
  import Alternate.Identity._

  type EndingSet = Set[Possessive]

  val One = Set(
    FirstPerson.singular(n∙i),
    SecondPerson.singular(s∙i),
    ThirdPerson.singular(n∙s∙a),
    FirstPerson.plural(m∙m∙e),
    SecondPerson.plural(n∙n∙e)
  )

  val Two = One ++ Set(ThirdPerson.singular(V∙n), ThirdPerson.plural(V∙n))
}

case class Possessive(
  person: Person,
  number: Number,
  ending: GraphemeMatcher
) extends TextualDescription {
  def toText = s"""${person.toText} ${number.toText} (${ending.toText})"""
}
