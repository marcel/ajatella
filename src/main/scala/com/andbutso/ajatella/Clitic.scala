package com.andbutso.ajatella

case class Clitic(ending: GraphemeMatcher) extends Suffix

object Clitic {
  import Alternate._
  import Consonants._
  import Vowels._
  import Alternate.Identity._

  // Common
  val kO   = Clitic(k∙O)
  val kin  = Clitic(k∙i∙n)
  val kAAn = Clitic(k∙A∙A∙n)
  val pA   = Clitic(p∙A)

  // Uncommon
  val kA   = Clitic(k∙A)
  val s    = Clitic(Consonants.s)

  val All = Seq(
    kO,
    kin,
    kAAn,
    pA,
    kA,
    s
  )
}
