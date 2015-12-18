package com.andbutso.ajatella

import scala.annotation.tailrec

case class Vowel(letter: Char) {
  @tailrec
  override final def equals(o: Any): Boolean = {
    o match {
      case vowel: Vowel =>
        equals(vowel.letter)
      case string: String =>
        letter.toString.toUpperCase.equals(string.toUpperCase)
      case char: Char =>
        letter.toUpper.equals(char.toUpper)
      case _ =>
        false
    }
  }

  override def hashCode = letter.toUpper.hashCode()
}

object Vowel {
  val a = Vowel('a')
  val o = Vowel('o')
  val u = Vowel('u')

  val e = Vowel('e')
  val i = Vowel('i')

  val ä = Vowel('ä')
  val ö = Vowel('ö')
  val y = Vowel('y')

  // N.B. Back and front vowels do not appear together, except across word boundaries in compound words
  val Back  = Set(a, o, u)
  val Front = Set(ä, ö, y)

  val All = Back ++ Front ++ Set(e, i)

  def isVowel(character: Char) = {
    All.contains(Vowel(character))
  }
}

// TODO Encode rules of vowel harmony dictated by front/back presence
// in the word stem