package com.andbutso.ajatella

object ConsonantGradation {
  import Grapheme.stringToGrapheme
  import Lexeme.stringToLexeme
  // TODO revisit this doesn't look quite right
  def shouldAlternate(stem: Lexeme, ending: String) = {
    // First rule: Does the word contain "p", "t", or "k". If not, there is no gradation.

    val string    = stem.string
    val syllables = stem.syllables

    (string.contains('p') || string.contains('t') || string.contains('k')) &&
      syllables.size > 1 &&
      syllables.last.takeRight(1).isVowel &&
      syllables.takeRight(2).toSet.size != 1 &&
      (ending.letters.filterNot { _.isVowel }.size <= 1 ||
        ending.letters.take(2).forall { _.isConsonant })
  }
}

// TODO gradation rules enumerated on page 32-44 of Karlsson
// N.B. Also useful explanation at http://www.lausti.com/articles/languages/finnishlanguage.htm
// in "Morphological complications" section
class ConsonantGradation {
  import Alternate._
  import Alternate.Identity._
  import Consonants._
  import Vowels._

  // Quantitative Gradation:
  // (1)
  // pp ~ p: kaappi ~ kaapi-ssa
  // (2)
  // tt ~ t: matto ~ mato-lla
  // (3)
  // kk ~ k: kukka ~ kuka-n
  val QuantitativeGradations = Map(
    "pp" -> "p",
    "tt" -> "t",
    "kk" -> "k"
  )

//  def apply(stem: Lexeme, ending: Case): String = {
//
//  }

  // TODO Gradation does not occur before the Illative ending
  def apply(stem: Lexeme, ending: String): String = {
    if (stem.syllables.size > 1) {
      QuantitativeGradations.foldLeft(stem) {
        case (transformedStem, (before, after)) =>
          transformedStem.replaceLast(before, after)
      } + ending
    } else {
      stem + ending
    }
  }

  type Transformer = Lexeme => Option[Lexeme]

  val Gradations = Seq[Transformer](
    // Quantitative Gradation:
    // #1 pp ~ p: kaappi ~ kaapi-ssa
    ( p ∙ p ).transform(_) { case Array(_, _) => Array("p") },
    // #2 tt ~ t: matto ~ mato-lla
    ( t ∙ t ).transform(_) { case Array(_, _) => Array("t") },
    // #3 kk ~ k: kukka ~ kuka-n
    ( k ∙ k ).transform(_) { case Array(_, _) => Array("k") },

    // Qualitative Gradation:
    ( (V|l|r) ∙ p ∙ !p ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, "v", rhs) }, // #4
    ( (V|h)   ∙ t ∙ !t ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, "d", rhs) }, // #5,#6
    ( (m|n)   ∙ p ∙ !p ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, lhs, rhs) }, // #8
    ( (m|n)   ∙ t ∙ !t ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, lhs, rhs) }, // #9
    ( m       ∙ k ∙ !k ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, lhs, rhs) }, // #10a
    ( n       ∙ k ∙ !k ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, "g", rhs) }, // #10b
    ( (l|r)   ∙ t ∙ !t ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, lhs, rhs) }, // #11,#12
    ( (l|r|h) ∙ k ∙  e ).transform(_) { case Array(lhs, _,   _) => Array(lhs, "j", "e") }, // #13,#14,#15
    ( U       ∙ k ∙  U ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, "v", rhs) }, // #16
    ( (V|l|r) ∙ k ∙ !k ).transform(_) { case Array(lhs, _, rhs) => Array(lhs, rhs)      }  // #7 (last b/c 13-16 have precedence)
  )

  val Gradate = Gradations.reduce { (lhs, rhs) =>
    (lexeme: Lexeme) => lhs(lexeme) orElse rhs(lexeme)
  }

  def baz(lexeme: Lexeme) = Gradate(lexeme) getOrElse(lexeme)

  // Qualitative Gradation:
  val Qualitative = Seq(
  // (4) short p changes to v after a vowel or l or r
    (V|l|r)∙p∙(!p),
  // p ~ v: tupa ~ tuva-ssa
  // (5,6) Short t changes to d after a vowel and after h
    (V|h)∙t∙(!t),
  // Vt ~ Vd: katu ~ kadu-lla
  // ht ~ hd: lähte- ~ lähde-n
  // (7) Short k disappears after a vowel, or l or r except special instances (13-16)
  // k ~  : tauko ~ tauo-n
  // (8-12) When short p, t, k occur after a nasal consonant with the same place of articulation (m, n, ŋ), or t
  // occurs after l or r (likewise w/ the same place of articulation), p, t, k are assimilated to the preceding consonant.
//    (m|n)∙((p∙(!p))|(t∙(!t))|(k∙(!k))),
    // mp ~ mm: ampu- ~ ammu-mme
  // nt ~ nn: ranta ~ ranna-lla
  // nk ~ ng: kenkä ~ kengä-n
    (l|r)∙(t∙(!t))
  // lt ~ ll: kulta ~ kulla-n
  // rt ~ rr: parta ~ parra-ssa
  // (13-16) Rare alternations apply to short k
  // lke ~ lje: polke- ~ polje-n
  // rke ~ rje: särke- ~ särje-n
  // hke ~ hje: rohkene-t ~ rohjet-a
  // (16) When k is preceded and followed by u-y (also suku, luku, kyky)
  // k ~ v: puku ~ puvu-n
  )
  // N.B. The exceptional words poika and aika (type (7)), lose k which makes i change to j in the weak grade.
}
