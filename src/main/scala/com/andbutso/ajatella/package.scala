package com.andbutso

package object ajatella {
  val Encoding = "ISO8859_4" // Northern European

  import org.kiama.output.PrettyPrinter._

  def pp(o: Any) = {
    println(pretty(any(o), w = 1))
  }

  def pe(objects: Seq[Any]): Unit = {
    objects foreach println
  }

  object Conversions {
    import Lexeme.stringToLexeme

    implicit def setOfStringToEntries(words: Set[String]): Entries = {
      Entries(words.flatMap { _.entry }.toIndexedSeq)
    }

    implicit def seqOfStringToEntries(words: Seq[String]): Entries = {
      Entries(words.flatMap { _.entry }.toIndexedSeq)
    }
  }
}
