package com.andbutso.ajatella

trait Grapheme {
  def letter: Char

  def isConsonant = {
    this match {
      case o: Consonant => true
      case _ => false
    }
  }

  def isVowel = {
    this match {
      case o: Vowel => true
      case _ => false
    }
  }
}

object Grapheme {
  def fromChar(character: Char): Grapheme = {
    if (Vowel.isVowel(character)) {
      Vowel(character)
    } else {
      Consonant(character)
    }
  }

  implicit def charToGrapheme(character: Char) = {
    Grapheme.fromChar(character)
  }

  implicit def stringToGrapheme(string: String) = {
    require(string.size == 1)

    Grapheme.fromChar(string.toCharArray.head)
  }
}