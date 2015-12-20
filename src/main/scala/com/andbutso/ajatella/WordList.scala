package com.andbutso.ajatella

object WordList {
  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  implicit val formats = DefaultFormats

  val dataRoot = System.getenv("AJATELLA") + "/data"
  val path = "kotus-sanalista_v1/kotus-sanalista_v1.tsv"
  val translationsPath = "fi_en_words_with_english_definition.json"

  lazy val list = WordList(loadWords.toSet)
  lazy val translations = loadTranslations

  def loadWords = {
    loadLinesFromFile(path) collect {
      case (Array(s,	hn,	av,	tn,	taivutus)) =>
        Word(s)
//        if (tn.size != 0) {
//          val pos = tn.toInt match {
//            case n if n <= 51 => PartOfSpeech.NOMINAL
//            case _ => POS.VERB
//          }
//
//          Word(s, pos)
//        } else {
//          Word(s, POS.UNKNOWN)
//        }
    }
  }

  def loadTranslations = {
    val data = io.Source.fromFile(makePath(translationsPath)).getLines.toArray.mkString
    val json = parse(data)
    Translations(json.extract[Seq[Translation]])
  }

  def loadLinesFromFile(name: String, skipHeader: Boolean = true, separator: String = "\t") = {
    val lines = io.Source.fromFile(
      makePath(name)
    ).getLines().toArray

    val linesToInclude = if (skipHeader) lines.tail else lines
    linesToInclude.map {
      _.split(separator, 5)
    }
  }

  def makePath(file: String) = s"$dataRoot/$file"
}

case class Sense(number: Int, text: String)
case class Translation(term: String, pos: Option[String], senses: Seq[Sense])

case class Translations(translations: Seq[Translation]) {
  lazy val morphemeIndex = translations.map { translation => translation.term -> translation } toMap

  def apply(morpheme: String) = {
    morphemeIndex.get(morpheme)
  }

  def apply(word: Word) = {
    morphemeIndex.get(word.morpheme)
  }

  def contains(morpheme: String) = {
    morphemeIndex.contains(morpheme)
  }
}

case class WordList(words: Set[Word]) {
  lazy val morphemeIndex = words.map { word => word.morpheme -> word } toMap

  def apply(morpheme: String) = {
    morphemeIndex(morpheme)
  }

  def contains(morpheme: String) = {
    morphemeIndex.contains(morpheme)
  }

//  def pos(pos: POS) = {
//    WordList(words filter { _.pos == pos })
//  }
}

//case class PartOfSpeech(pos: String) // tmp
object PartOfSpeech {
  val Adjective    = "Adjective"
  val Adverb       = "Adverb"
  val Conjunction  = "Conjunction"
  val Interjection = "Interjection"
  val Letter       = "Letter"
  val Noun         = "Noun"
  val Numeral      = "Numeral"
  val Particle     = "Particle"
  val Postposition = "Postposition"
  val Preposition  = "Preposition"
  val Pronoun      = "Pronoun"
  val ProperNoun   = "Proper noun"
  val Suffix       = "Suffix"
  val Verb         = "Verb"
}

case class Word(morpheme: String) extends Ordered[Word] {
  override def compare(otherWord: Word) = {
    morpheme.compareTo(otherWord.morpheme)
  }
}
