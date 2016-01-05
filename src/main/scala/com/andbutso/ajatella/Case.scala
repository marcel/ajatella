package com.andbutso.ajatella

object Case {
  import Alternate._
  import Lexeme._
  import Letters._

  def detect(lexeme: Lexeme): Case = {
    if (WordList.list.contains(lexeme)) {
      Nominative
    } else {
      All.flatMap { possibleCase =>
        possibleCase.suffixes.collect {
          case suffix if lexeme ~= suffix.toString(lexeme) =>
            (possibleCase, suffix.toString(lexeme))
        }
      }.maxBy { case (c, s) => s.size }._1
    }
  }
//
//  def apply(letters: Letters): Case = {
//    new Case(Set(letters))
//  }

  // Grammatical
  case object Nominative extends Case {
    val suffixes = Set(Letters(""))
  }
  case object Genetive   extends Case(Some("of")) {
    val n = Letters("n")
    val suffixes = Set(n)
  }
  case object Accusative extends Case(Some("(object, whole)")) {
    val t = Letters("t")
    val suffixes = Set(t)
  }
  case object Partative  extends Case(Some("(object, part/incomplete)")) {
    import com.andbutso.ajatella.{Alternate => Alt}

    val A   = Letters(Alt.A)
    val tA  = Letters("t", Alt.A)
    val ttA = Letters("tt", Alt.A)

    val suffixes = Set(A, tA, ttA)

    override def suffixFor(lexeme: Lexeme) = {
      if (lexeme.letters.last.isVowel) {
        Letters(A)
      } else {
        Letters("t", A)
      }
    }
  }

  // Locative (internal)
  case object Inessive extends Case(Some("in")) {
    val ssA = Letters("ss", A)
    val suffixes = Set(ssA)
  }
  case object Elative  extends Case(Some("from (inside)")) {
    val stA = Letters("st", A)
    val suffixes = Set(stA)
  }
  case object Illative extends Case(Some("into")) {
    val Vn   = Letters(V, "n")
    val hVn  = Letters("h", V, "n")
    val seen = Letters("seen")
    val siin = Letters("siin") // Plural

    val suffixes = Set(Vn, hVn, seen, siin)

    override def suffixFor(lexeme: Lexeme) = {
      val lastLetter = lexeme.letters.last

      if (lastLetter.isVowel) {
        Letters(lastLetter.letter, "n")
      } else {
        Letters("h", lastLetter.letter, "n")
      }
    }
  }

  // Locative (external)
  case object Adessive extends Case(Some("at, on")) {
    val llA = Letters("ll", A)
    val suffixes = Set(llA)
  }
  case object Ablative extends Case(Some("from")) {
    val ltA = Letters("lt", A)
    val suffixes = Set(ltA)
  }
  case object Allative extends Case(Some("to (outside), onto")) {
    val lle = Letters("lle")
    val suffixes = Set(lle)
  }

  // Essive
  case object Essive extends Case(Some("as")) {
    val nA = Letters("n", A)
    val suffixes = Set(nA)
  }
  case object Translative extends Case(Some("into (change, transformation, not movement)")) {
    val ksi = Letters("ksi")
    val suffixes = Set(ksi)
  }

  // Marginal
  case object Comitative extends Case(Some("together (with)")) {
    val ine = Letters("ine")
    val suffixes = Set(ine)
  }
  case object Abessive extends Case(Some("without")) {
    val ttA = Letters("tt", A)
    val suffixes = Set(ttA)
  }
  // N.B. Singular form extremely rare
  case object Instructive extends Case {
    val n = Letters("n")
    val suffixes = Set(n)
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
  def suffixes: Set[Letters]

  def description = {
    val endings = suffixes.map { "-" + _.toString }.mkString(", ")

    (this.toString, endings, preposition)
  }

  def letters = ??? // TODO Choose the right suffix

  def matches(lexeme: Lexeme) = {
    suffixes.exists { suffix =>
      lexeme.contains(suffix.toString(lexeme))
    }
  }

  def suffixFor(lexeme: Lexeme) = {
    suffixes.head
  }

  def :+:(lexeme: Lexeme): Lexeme = {
    val cg = new ConsonantGradation
    // TODO Deal with when letters.size is greater than 1
    cg(lexeme, suffixFor(lexeme).toString(lexeme))
  }
}




