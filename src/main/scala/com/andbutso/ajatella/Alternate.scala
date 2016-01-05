package com.andbutso.ajatella

import scala.annotation.tailrec

trait Alternate

object Alternate {
  import Grapheme.charToGrapheme

  case object Letter extends Alternate {
    override def equals(o: Any): Boolean = {
      o match {
        // TODO Constrain to just the alphabet, i.e. punctuation should return false
        case c: Char => true
        case s: String => true
        case g: Grapheme => true
        case _ => hashCode().equals(o.hashCode())

      }
    }
  }

  case object Vowel extends Alternate {
    override def equals(o: Any): Boolean = {
      o match {
        case c: Char =>
          c.isVowel
        case s: String =>
          s.size == 1 && s.toCharArray.head.isVowel
        case v: com.andbutso.ajatella.Vowel =>
          true
        case _ =>
          hashCode().equals(o.hashCode())
      }
    }
  }

  case object Consonant extends Alternate {
    override def equals(o: Any): Boolean = {
      o match {
        case c: Char =>
          c.isConsonant
        case s: String =>
          s.size == 1 && s.toCharArray.head.isConsonant
        case c: com.andbutso.ajatella.Consonant =>
          true
        case _ =>
          hashCode().equals(o.hashCode())
      }
    }
  }

  case class Not(grapheme: Grapheme) extends Alternate {
    override def equals(o: Any): Boolean = {
      o match {
        case c: Char => grapheme != c
        case s: String => grapheme != s
        case g: Grapheme => grapheme != g
        case _ => true
      }
    }
  }

  case object VowelOrConsonant extends Alternate
  case object ConsecutiveVowels extends Alternate
  case object ConsonantFollowedByVowel extends Alternate

  // TODO fix naming/taxonomy. Aside from the vowel harmony alternates the rest
  // aren't really alternates as they are letter sequence sets which dictate the type
  // and sequence of one or more letters
  val L = Letter
  val V = Vowel
  val C = Consonant
  val A = VowelHarmonyAlternate('a', 'ä')
  val O = VowelHarmonyAlternate('o', 'ö')
  val U = VowelHarmonyAlternate('u', 'y')

  val VorC = VowelOrConsonant
  val VV   = ConsecutiveVowels
  val CV   = ConsonantFollowedByVowel
  // TODO Implement `or` on whatever Alternate becomes so you can compose
  // new placeholder matchers like "CV or VV" or replace "VorC" with just "V or C"
}

case class VowelHarmonyAlternate(back: Vowel, front: Vowel) extends Alternate {
  def apply(vowelType: Vowel.Type) = {
    vowelType match {
      case Vowel.Back  => back
      case Vowel.Front => front
      case Vowel.Neutral =>
        throw new IllegalArgumentException("Vowel type must be Front or Back, was Neutral")
    }
  }

  @tailrec
  override final def equals(o: Any): Boolean = {
    o match {
      case c: Char =>
        val v = Vowel(c)
        v == back || v == front
      case Vowel(v) =>
        equals(v)
      case vha: VowelHarmonyAlternate =>
        super.equals(vha)
      case _ =>
        false
    }
  }
}