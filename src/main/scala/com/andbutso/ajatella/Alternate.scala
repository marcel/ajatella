package com.andbutso.ajatella


trait Alternate

object Alternate {
  import com.andbutso.ajatella

  case object Vowel extends Alternate {
    override def equals(o: Any): Boolean = {
      o match {
        case c: Char =>
          ajatella.Vowel.isVowel(c)
        case s: String =>
          s.size == 1 && ajatella.Vowel.isVowel(s.toCharArray.head)
        case `Vowel` =>
          true
        case _ =>
          false
      }
    }
  }

  case object Consonant extends Alternate

  val V = Vowel
  val C = Consonant
  val A = VowelHarmonyAlternate('a', 'ä')
  val O = VowelHarmonyAlternate('o', 'ö')
  val U = VowelHarmonyAlternate('u', 'y')
}

case class VowelHarmonyAlternate(alternates: (Char, Char)) extends Alternate {
  override def equals(o: Any): Boolean = {
    o match {
      case c: Char =>
        Vowel(c).equals(Vowel(alternates._1)) || Vowel(c).equals(Vowel(alternates._2))
      case vha: VowelHarmonyAlternate =>
        super.equals(vha)
      case _ =>
        false
    }
  }
}