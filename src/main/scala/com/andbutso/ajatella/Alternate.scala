package com.andbutso.ajatella


trait Alternate

object Alternate {
  case object Vowel extends Alternate
  case object Consonant extends Alternate

  val V = Vowel
  val C = Consonant
  val A = VowelHarmonyAlternate('a', 'ä')
  val O = VowelHarmonyAlternate('o', 'ö')
  val U = VowelHarmonyAlternate('u', 'y')
}

case class VowelHarmonyAlternate(alternates: (Char, Char)) extends Alternate
