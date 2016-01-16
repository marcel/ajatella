package com.andbutso.ajatella

case class Lemma(lexeme: Lexeme, tags: String)

case class Lexeme(val string: String) extends AnyVal {
  import Grapheme.charToGrapheme
  import Grapheme.stringToGrapheme
  import Lexeme.stringToLexeme

  override def toString = string

  def syllables = {
    SyllableBoundary.split(string)
  }

  def letters = {
    string.toCharArray map { char =>
      Grapheme.fromChar(char)
    }
  }

  def hasVowelEnding = {
    letters.last.isVowel
  }

  def hasConsonantEnding = {
    letters.last.isConsonant
  }

  // TODO Implement w/ lemmatizer
  def lemma2 = {
    val pattern = """<pre>.*\s+"([^"]+)"\s+(.*)</pre>""".r
    val query   = java.net.URLEncoder.encode(string, Encoding)
    val url     = s"http://www2.lingsoft.fi/cgi-bin/fintwol?word=$query"

    val data = io.Source.fromURL(url, Encoding).getLines.toArray.mkString
    pattern.findFirstMatchIn(data) map { regexMatch =>
      Lemma(regexMatch.group(1), regexMatch.group(2))
    }
  }

  def lemma = {
    entry.map { _.word }
  }

  def decompound = {
    Decompounder(string)
  }

  def form = {
    WordList.entries.form(string)
  }

  def inflected(nounCase: Case) = {
    WordList.entries(string) flatMap { entry =>
      entry.inflected(nounCase)
    }
  }

  def isBasicForm = {
    WordList.list.contains(string) || WordList.entries.form(string).exists { _ == "lemma" }
  }

  def endsWith(foo: GraphemeMatcher) = {
    val ending = string.takeRight(foo.indexes.size)
    foo.matches(ending)
  }

  def translation = {
    WordList.translations(string)
  }

  def entry = {
    WordList.entries(string)
  }

  def senses: Seq[Sense] = {
    translation.map { _.senses }.getOrElse(Seq.empty) //{
//      lemma.map {
//        _.lexeme.senses
//      }.getOrElse(Seq.empty)
//    }
  }

  def definitions = {
    entry.map { _.definitions }.getOrElse(Seq.empty)
  }

  def define = definitions

  def drop(suffix: String): Option[Lexeme] = {
    if (string.endsWith(suffix)) {
      val prefixSize = string.size - suffix.length
      Some(string.take(prefixSize))
    } else {
      None
    }
  }

  def drop(suffix: Letters): Option[Lexeme] = {
    drop(suffix.toString(this))
  }

  def partOfSpeech = {
    translation flatMap { _.pos }
  }

  def frequencyRank = {
    WordList.wordFrequencies.rank(this)
  }

  def vowelTypes: Set[Vowel.Type] = {
    letters.collect { case v: Vowel => v.vowelType } toSet
  }

  def vowelType = {
    val types = vowelTypes - Vowel.Neutral
    // TODO Deal with compound words that contain both front and back vowels
    if (types.size > 1) {
      throw new UnsupportedOperationException("Word contains both front and back vowels")
    } else {
      // If only neutral vowels are present then the word takes the front form
      if (types.isEmpty) {
        Vowel.Front
      } else {
        types.head
      }
    }
  }

  def replaceLast(before: String, after: String): Lexeme = {
    string.reverse.replaceFirst(before, after).reverse
  }

  def +(ending: Case): Lexeme = {
    string + ending
  }

  // e.g.
  //  scala> Lexeme("kappi") ~= Letters(C, V)
  //  res4: Boolean = true
  def ~=(pattern: Letters) = {
    letters.takeRight(pattern.letters.size).mkString =~ pattern
  }

  def =~(pattern: Letters) = {
    val lexemeLetters = letters.toIndexedSeq

    if (pattern.letters.isEmpty || pattern.letters.size > lexemeLetters.size ) {
      false
    } else {
      pattern.letters.zipWithIndex.forall {
        case (Left(string), index) =>
          lexemeLetters(index) == string
        case (Right(alternate), index) =>
          alternate == lexemeLetters(index)
      }
    }
  }
}

object Lexeme {
  implicit def stringToLexeme(string: String): Lexeme = {
    Lexeme(string)
  }

  implicit def lexemeToString(lexeme: Lexeme): String = {
    lexeme.string
  }
}