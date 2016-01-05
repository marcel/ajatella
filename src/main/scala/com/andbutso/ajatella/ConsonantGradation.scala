package com.andbutso.ajatella

object ConsonantGradation {
  import Grapheme.stringToGrapheme
  import Lexeme.stringToLexeme

  def shouldAlternate(stem: Lexeme, ending: String) = {
    val syllables = stem.syllables

    syllables.size > 1 &&
      syllables.last.isVowel &&
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

//  def foo = {
//    val bar = ""
//
//    L(V, 'p', Not('p')) | L('l', 'p', Not('p')) | L('r', 'p', Not('p')) {
//      case L(x, p) => L(x, 'v')
//    },
//    L(V, 't', Not('t')) | L('h', 't', Not('t')) {
//      case L(x, t, notT) => L(x, "d", notT)
//    },
//    L(V, 'k', Not('k')) | L('l', 'k', Not('k')) | L('r', 'k', Not('k')) {
//      case L(x, k, notK) => L(x, notK)
//    }
//  }

  // Qualitative Gradation:
  // (4) short p changes to v after a vowel or l or r
  // p ~ v: tupa ~ tuva-ssa
  // (5,6) Short t changes to d after a vowel and after h
  // Vt ~ Vd: katu ~ kadu-lla
  // ht ~ hd: lähte- ~ lähde-n
  // (7) Short k disappears after a vowel, or l or r except special instances (13-16)
  // k ~  : tauko ~ tauo-n
  // (8-12) When short p, t, k occur after a nasal consonant with the same place of articulation (m, n, ŋ), or t
  // occurs after l or r (likewise w/ the same place of articulation), p, t, k are assimilated to the preceding consonant.
  // mp ~ mm: ampu- ~ ammu-mme
  // nt ~ nn: ranta ~ ranna-lla
  // nk ~ ng: kenkä ~ kengä-n
  // lt ~ ll: kulta ~ kulla-n
  // rt ~ rr: parta ~ parra-ssa
  // (13-16) Rare alternations apply to short k
  // lke ~ lje: polke- ~ polje-n
  // rke ~ rje: särke- ~ särje-n
  // hke ~ hje: rohkene-t ~ rohjet-a
  // (16) When k is preceded and followed by u-y (also suku, luku, kyky)
  // k ~ v: puku ~ puvu-n

  // N.B. The exceptional words poika and aika (type (7)), lose k which makes i change to j in the weak grade.
}
