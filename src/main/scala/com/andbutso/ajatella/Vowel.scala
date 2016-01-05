package com.andbutso.ajatella

import scala.annotation.tailrec

case class Vowel(letter: Char) extends Grapheme {
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

  def isBack = {
    Vowel.Backs.contains(this)
  }

  def isFront = {
    Vowel.Fronts.contains(this)
  }

  def isNeutral = {
    Vowel.Neutrals.contains(this)
  }

  def vowelType = {
    if (isBack) {
      Vowel.Back
    } else if (isFront) {
      Vowel.Front
    } else {
      Vowel.Neutral
    }
  }

  override def hashCode = letter.toUpper.hashCode()
}

object Vowel {
  trait Type
  case object Front extends Type
  case object Back extends Type
  case object Neutral extends Type

  // Back
  val a = Vowel('a')
  val o = Vowel('o')
  val u = Vowel('u')

  // Neutral
  val e = Vowel('e')
  val i = Vowel('i')

  // Front
  val ä = Vowel('ä')
  val ö = Vowel('ö')
  val y = Vowel('y')

  // N.B. Back and front vowels do not appear together,
  // except across word boundaries in compound words
  val Backs    = Set(a, o, u)
  val Fronts   = Set(ä, ö, y)
  val Neutrals = Set(e, i)

  val All = Backs ++ Fronts ++ Neutrals

  def isVowel(character: Char) = {
    All.contains(Vowel(character))
  }

  implicit def vowelToChar(vowel: Vowel): Char = {
    vowel.letter
  }

  implicit def charToVowel(char: Char): Vowel = {
    require(isVowel(char), s"'$char' must be a vowel to convert to a vowel")

    Vowel(char)
  }
}

// TODO Encode rules of vowel harmony dictated by front/back presence
// in the word stem