package com.andbutso.ajatella

case class Lexeme(string: String) {
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

  def lemma = ??? // TODO Implement w/ lemmatizer

  def translation = {
    WordList.translations(string)
  }

  def senses = {
    translation map { _.senses } getOrElse(Seq.empty)
  }

  def partOfSpeech = {
    translation flatMap { _.pos }
  }

  def frequencyRank = {
    WordList.wordFrequencies.rank(this)
  }
}

object Lexeme {
  implicit def stringToMorpheme(string: String): Lexeme = {
    Lexeme(string)
  }

  implicit def morphemeToString(morpheme: Lexeme): String = {
    morpheme.string
  }
}