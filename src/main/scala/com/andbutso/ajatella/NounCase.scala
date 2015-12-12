package com.andbutso.ajatella


object NounCase {
  // TODO Implement NounCase.detect
  def detect(word: String) = {
    Seq.empty[NounCase]
  }

  val All = Set(
    // Grammatical
    Nominative,
    Genetive,
    Accusative,
    Partative,
    // Locative (internal)
    Inessive,
    Elative,
    Illative,
    // Locative (external)
    Adessive,
    Ablative,
    Allative,
    // Essive
    Essive,
    Exessive,
    Translative,
    // Marginal
    Instructive,
    Abessive,
    Comitative
  )
}

trait NounCase {
  def suffix: Set[String]
}

// Grammatical
case object Nominative extends NounCase {
  val suffix = Set("")
}

case object Genetive extends NounCase {
  val suffix = Set("n")
}

case object Accusative extends NounCase {
  val suffix = Set("", "t", "n")
}

case object Partative extends NounCase {
  val suffix = Set("ta", "a")
}

// Locative (internal)
case object Inessive extends NounCase {
  val suffix = Set("ssa")
}

case object Elative extends NounCase {
  val suffix = Set("sta")
}

case object Illative extends NounCase {
  val suffix = Set("an", "en", "on", "in", "han", "hen", "hon", "hin") // TODO Verify correctness
}

// Locative (external)
case object Adessive extends NounCase {
  val suffix = Set("lla")
}

case object Ablative extends NounCase {
  val suffix = Set("lta")
}

case object Allative extends NounCase {
  val suffix = Set("lle")
}

// Essive
case object Essive extends NounCase {
  val suffix = Set("na")
}

case object Exessive extends NounCase {
  val suffix = Set("nta")
}

case object Translative extends NounCase {
  val suffix = Set("ksi")
}

// Marginal
case object Instructive extends NounCase {
  val suffix = Set("n")
}

case object Abessive extends NounCase {
  val suffix = Set("tta")
}

case object Comitative extends NounCase {
  val suffix = Set("ne") // TODO Technically not a suffix, e.g. "taloineen", "with the house"
}




