package com.andbutso.ajatella

case class Morpheme(string: String) {
  import Vowel.charToVowel

  def syllables = {
    SyllableBoundary.split(string)
  }

  def letters = {
    string.toCharArray
  }

  def hasVowelEnding = {
    letters.last.isVowel
  }

  def hasConsonantEnding = {
    letters.last.isConsonant
  }

  def translation = {
    WordList.translations(string)
  }

  def senses = {
    translation map { _.senses } getOrElse(Seq.empty)
  }
}

object Morpheme {
  implicit def stringToMorpheme(string: String): Morpheme = {
    Morpheme(string)
  }

  implicit def morphemeToString(morpheme: Morpheme): String = {
    morpheme.string
  }
}