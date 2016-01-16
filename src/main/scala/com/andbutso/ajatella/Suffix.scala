package com.andbutso.ajatella

object Suffix {
  val All = {
    Case.All ++ Nominal.DerivationSuffix.All
  }
}

// TODO If/when suffix is molded into the right generalization then probably the
// :+: method on Case should be moved up into Suffix so that appending any type of suffix
// onto a lexeme performs consonant gradation if necessary and selects
// the appropriate front or back vowel if applicable
trait Suffix {
  def letters: Letters
}


// Deverbal and Denominal suffixes
object DerivationSuffix {

  // Nouns
  //
  //  -LA:	A PLACE
  //  ruokala	canteen
  //  mummola	"grandmothers place"
  //
  //  -LLINEN:	"FULL OF SOMETHING"
  //  kourallinen	a handful
  //  lastillinen	a truck-load
  //  laatikollinen	a boxful
  //
  //  -NEN:	"SMALL THING"
  //  kalanen	a little fish
  //  tyttönen	a little girl
  //
  //  -TAR:	FEMINE
  //  kuningatar	queen
  //  laulajatar	a songstress, female singer
  //  tarjoilijatar	a waitress

  //  Verbs become nouns
  //
  //  -JA:	"WHO DOES SOMETHING"
  //  laulaja	a singer
  //  kävelijä	a walker, pedestrian
  //  juoksija	a runner

  //  Nouns become adjectives
  //
  //  -LAINEN:	WHAT KIND OF, NATIONALITY
  //  suomalainen	finnish, a Finn
  //  englantilainen	english, an Englisman
  //  tyhmänlainen	foolish
  //  sellainen	"that kind of"
  //
  //  -LLINEN:	related to something
  //  kuninkaallinen	royal
  //
  //  -MAINEN:	WHAT KIND OF
  //  tyttömäinen	girly
  //  kuitumainen	fibre-like

  //  Adverbs
  //
  //  -TON:	"WITHOUT SOMETHING"
  //  painoton	weightless
  //  iloton	cheerless
  //  pennitön	penniless
  //
  //  -ISIN:	HABITUAL
  //  aamuisin	in the mornings
  //  maanantaisin	every monday
  //
  //  -STI:	ADVERBS EXPRESSING HOW
  //  kauniisti	beautifully, "in a beautiful way"
  //  nopeasti	fast

  //  Verbs
  //
  //  -ISTA:	KAUSATIVE
  //  kaunistaa	to make beautiful
  //  jalostaa	to refine, to sophisticate
  //
  //  -ISTU:	REFLEKSIVE
  //  laiskistua	to become lazy
  //
  //  -SKELLA	FREQUENT
  //  istuskella	to sit around
  //
  //  -NE:	TO BECOME SOMETHING GRADUALLY
  //  pienetä	diminish
  //  kylmetä	to cool down

}

class DerivationSuffix(val letters: Letters) extends Suffix