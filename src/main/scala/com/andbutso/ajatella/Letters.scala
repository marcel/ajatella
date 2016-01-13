package com.andbutso.ajatella

import scala.annotation.tailrec

object Letters {
  def apply(letters: Any*): Letters = {
    new Letters(
      letters.collect {
        case char: Char => Left(char.toString)
        case string: String => Left(string)
        case grapheme: Grapheme => Left(grapheme.letter.toString)
        case alt: Alternate => Right(alt)
        case _ => throw new IllegalArgumentException("Letter must be String or Alternate")
      }
    )
  }

  implicit def stringToLetters(string: String): Letters = {
    Letters(string.toCharArray: _*)
  }
}

class Letters(val letters: Seq[Either[String, Alternate]]) {
  import Lexeme.stringToLexeme

  def toString(lexeme: Lexeme) = {
    letters map { letter =>
      letter match {
        case Left(string) =>
          string
        case Right(alternate) =>
          alternate match {
            case vha: VowelHarmonyAlternate =>
              vha(lexeme.vowelType)
            case Alternate.V =>
              lexeme.letters.filter { _.isVowel }.last
          }
      }
    } mkString
  }

  @tailrec
  override final def equals(o: Any) = {
    o match {
      case l: Letters => letters.zip(l.letters) forall { // TODO De-jank
        case (Left(a), Left(b)) => a == b
        case (Left(a), Right(b)) => a == b
        case (Right(a), Left(b)) => a == b
        case (Right(a), Right(b)) => a == b
      }
      case s: String => equals(Letters(s.letters: _*))
      case g: Grapheme => equals(Letters(g))
      case _ => super.equals(o)
    }
  }

  override def toString = {
    letters map { letter =>
      letter match {
        case Left(string) =>
          string
        case Right(alternate) =>
          alternate match {
            case Alternate.A => "A"
            case Alternate.U => "U"
            case Alternate.O => "O"
            case Alternate.V => "V"
          }
      }
    } mkString
  }
}