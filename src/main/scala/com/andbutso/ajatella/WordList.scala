package com.andbutso.ajatella

object WordList {
  import org.json4s._
  import org.json4s.jackson.JsonMethods._
  implicit val formats = DefaultFormats

  val dataRoot = System.getenv("AJATELLA") + "/data"
  val path = "kotus-sanalista_v1/kotus-sanalista_v1.tsv"
  val translationsPath = "fi_en_words_with_english_definition.json"
  val partOfSpeechPath = "lexemes.tsv"
  val wiktionaryPath   = "fi_en_wiktionary.json"

  lazy val list = WordList(loadWords.toSet)
  lazy val translations = loadTranslations
  lazy val entries = loadWiktionary

  lazy val partOfSpeechIndex = {
    loadLinesFromFile(partOfSpeechPath).collect {
      case (Array(word, _, partOfSpeech, _)) =>
        val pos = partOfSpeech.split('_').head
        word -> pos
    }.toMap
  }

  case class WordFrequency(words: Seq[String]) {
    lazy val toRank = words.zipWithIndex.toMap

    def rank(word: String) = {
      toRank.get(word) getOrElse Int.MaxValue
    }
  }

  lazy val wordFrequencies = {
    WordFrequency(
      loadLinesFromFile("9996-most-common-lemmas-from-Finnish-printed-news.txt") map { _.head }
    )
  }

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

  def loadWiktionary = {
    val data = io.Source.fromFile(makePath(wiktionaryPath)).getLines.toArray.mkString
    val json = parse(data)
    Entries(json.camelizeKeys.extract[Seq[Entry]])
  }

  def loadLinesFromFile(name: String, skipHeader: Boolean = true, separator: String = "\t") = {
    val lines = io.Source.fromFile(
      makePath(name)
    ).getLines().toArray

    val linesToInclude = if (skipHeader) lines.tail else lines
    linesToInclude.filterNot { _.startsWith("#") }.map {
      _.split(separator, 5)
    }
  }

  def makePath(file: String) = s"$dataRoot/$file"
}

case class Entries(entries: Seq[Entry]) {
  lazy val completeLookup = {
    val formToIndex = collection.mutable.Map[String, Int]()

    entries.zipWithIndex.foreach { case (entry, index) =>
      entry.conjugations.foreach { conjugation =>
        formToIndex(conjugation.positive) = index
        formToIndex(conjugation.negative) = index
      }

      entry.declensions.foreach { inflectedForm =>
        formToIndex(inflectedForm.singular) = index
        formToIndex(inflectedForm.plural) = index
      }
    }

    formToIndex
  }

  lazy val lemmaLookup = entries.map { entry => entry.word -> entry }.toMap

  def apply(word: String) = {
    lemmaLookup.get(word) orElse {
      completeLookup.get(word) map { index =>
        entries(index)
      }
    }
  }

  lazy val wordTree = {
    val wt = PrefixSearchTree()
    entries foreach { entry => wt.add(entry.word) }
    wt
  }

  lazy val reverseWordTree = {
    val wt = PrefixSearchTree()
    entries foreach { entry => wt.add(entry.word.reverse) }
    wt
  }

  def startsWith(prefix: String) = {
    wordTree.find(prefix) map { path =>
      path.words
    } getOrElse(Set.empty)
  }

  def entriesStartingWith(prefix: String) = {
    startsWith(prefix) flatMap { word => apply(word) }
  }

  def endsWith(suffix: String) = {
    reverseWordTree.find(suffix.reverse) map { path =>
      path.words.map { _.reverse }
    } getOrElse(Set.empty)
  }

  def entriesEndingWith(suffix: String) = {
    startsWith(suffix.reverse) flatMap { word => apply(word) }
  }
}

case class Entry(
  word: String,
  definitions: Seq[Definition],
  declensions: Seq[InflectedForm],
  conjugations: Seq[Conjugation],
  conjugationType: Option[String],
  declensionType: Option[String],
  pronunciation: Option[String],
  hyphenation: Option[String],
  relatedTerms: Seq[String],
  derivedTerms: Map[String, Seq[String]],
  seeAlso: Seq[String]
) {
  import org.kiama.output.PrettyPrinter._

  def inflected(nounCase: Case) = {
    declensions.find { _.caseName == nounCase.name }
  }

  def formatted = {
    pretty(any(this), w = 1)
  }
}
case class InflectedForm(caseName: String, singular: String, plural: String)
case class Conjugation(mood: String, tense: String, person: String, number: Option[String], positive: String, negative: String)

case class Definition(partOfSpeech: String, number: Int, text: String, examples: Seq[ExampleUsage])
case class ExampleUsage(finnish: String, english: String)

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

  lazy val wordTree = {
    val wt = PrefixSearchTree()
    words foreach { word => wt.add(word.morpheme) }
    wt
  }

  lazy val reverseWordTree = {
    val wt = PrefixSearchTree()
    words foreach { word => wt.add(word.morpheme.reverse) }
    wt
  }

  def startsWith(prefix: String) = {
    wordTree.find(prefix) map { path =>
      path.words
    } getOrElse(Set.empty)
  }

  def endsWith(suffix: String) = {
    reverseWordTree.find(suffix.reverse) map { path =>
      path.words.map { _.reverse }
    } getOrElse(Set.empty)
  }

  def apply(morpheme: String) = {
    morphemeIndex(morpheme)
  }

  def contains(morpheme: String) = {
    morphemeIndex.contains(morpheme)
  }
//  toSeq.sortBy { w => (w.frequencyRank, w.size)}.filterNot { w => w.senses.isEmpty} foreach { w => println(w, w.senses, if (w.frequencyRank == Int.MaxValue) 0 else w.frequencyRank) }
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
