package com.andbutso.ajatella

case class Clitic(letters: Letters) extends Suffix

object Clitic {
  import Alternate._

  // Common
  val kO   = Clitic(Letters("k", O))
  val kin  = Clitic(Letters("kin"))
  val kAAn = Clitic(Letters("k", A, A, "n"))
  val pA   = Clitic(Letters("p", A))

  // Uncommon
  val kA   = Clitic(Letters("k", A))
  val s    = Clitic(Letters("s"))
}
