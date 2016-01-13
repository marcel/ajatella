package com.andbutso.ajatella

object Case {
  import Alternate._
  import Lexeme._
  import Letters._
  import Consonants._
  import Vowels._
  import Alternate.Identity._
//
//  def detect(lexeme: Lexeme): Case = {
//    if (WordList.list.contains(lexeme)) {
//      Nominative
//    } else {
//      All.flatMap { possibleCase =>
//        possibleCase.suffixes.collect {
//          case suffix if lexeme ~= suffix(lexeme) =>
//            (possibleCase, suffix(lexeme))
//        }
//      }.maxBy { case (c, s) => s.size }._1
//    }
//  }

  // Grammatical
  case object Nominative extends Case {
    def suffixes = Set(V∙C)
  }
  case object Genetive extends Case(Some("of")) {
    def suffixes = Set(n)
  }
  case object Accusative extends Case(Some("(object, whole)")) {
    def suffixes = Set(t)
  }
  case object Partative  extends Case(Some("(object, part/incomplete)")) {
    def suffixes = Set(A, t∙A, t∙t∙A)

    override def suffixFor(lexeme: Lexeme) = {
      if (lexeme.letters.last.isVowel) {
        A
      } else {
        t∙A
      }
    }
  }

  // Locative (internal)
  case object Inessive extends Case(Some("in")) {
    val ssA = s∙s∙A
    def suffixes = Set(s∙s∙A)
  }
  case object Elative  extends Case(Some("from (inside)")) {
    val stA = s∙t∙A
    def suffixes = Set(stA)
  }
  case object Illative extends Case(Some("into")) {
    def suffixes = Set(V∙n, h∙V∙n, s∙e∙e∙n, s∙i∙i∙n)

    override def suffixFor(lexeme: Lexeme) = {
      val lastLetter = lexeme.letters.last

      if (lastLetter.isVowel) {
        lastLetter.letter ∙ n
      } else {
        Letters("h", lastLetter.letter, "n")
        h ∙ lastLetter.letter ∙ n
      }
    }
  }

  // Locative (external)
  case object Adessive extends Case(Some("at, on")) {
    val llA = l∙l∙A
    def suffixes = Set(llA)
  }
  case object Ablative extends Case(Some("from")) {
    val ltA = l∙t∙A
    def suffixes = Set(ltA)
  }
  case object Allative extends Case(Some("to (outside), onto")) {
    val lle = l∙l∙e
    def suffixes = Set(lle)
  }

  // Essive
  case object Essive extends Case(Some("as")) {
    val nA = n∙A
    def suffixes = Set(nA)
  }
  case object Translative extends Case(Some("into (change, transformation, not movement)")) {
    val ksi = k∙s∙i
    def suffixes = Set(ksi)
  }

  // Marginal
  case object Comitative extends Case(Some("together (with)")) {
    val ine = i∙n∙e
    def suffixes = Set(ine)
  }
  case object Abessive extends Case(Some("without")) {
    val ttA = t∙t∙A
    def suffixes = Set(ttA)
  }
  // N.B. Singular form extremely rare
  case object Instructive extends Case {
    def suffixes = Set(n)
  }

  // Frequency distribution here: https://www.cs.tut.fi/~jkorpela/finnish-cases.html
  val All = Seq(
    // Grammatical
    Nominative,
    Genetive,
    Accusative,
    Partative,
    // Locative (internal)
    Inessive,
    Elative,
    Illative,
    // Locative (external)
    Adessive,
    Ablative,
    Allative,
    // Essive
    Essive,
    Translative,
    // Marginal
    Instructive,
    Abessive,
    Comitative
  )

  def table = {
    val descriptions = All map { _.description }
    val padding      = descriptions.map { _._1.size }.max
    descriptions map {
      case (name, endings, preposition) =>
        val paddedName = name.reverse.padTo(padding, " ").reverse.mkString
        s"""$paddedName: $endings [${preposition.getOrElse("-")}]"""
    } foreach println
  }
}

abstract class Case(preposition: Option[String] = None) extends Suffix {
  def suffixes: Set[GraphemeMatcher]

  def name = {
    getClass.getSimpleName.toLowerCase.replace("$", "")
  }

  def description = {
    val endings = suffixes.map { "-" + _.toString }.mkString(", ")

    (this.toString, endings, preposition)
  }

  def letters = ??? // TODO Choose the right suffix

  // TODO Revisit
//  def matches(lexeme: Lexeme) = {
//    suffixes.exists { suffix =>
//      lexeme.endsWith(suffix(lexeme))
//    }
//  }

  def suffixFor(lexeme: Lexeme) = {
    suffixes.head
  }

  // TODO Revisit
  def :+:(lexeme: Lexeme): Lexeme = {
    val cg = new ConsonantGradation
    // TODO Deal with when letters.size is greater than 1
    cg.baz(lexeme + suffixFor(lexeme).interpolate(lexeme))
  }
}




