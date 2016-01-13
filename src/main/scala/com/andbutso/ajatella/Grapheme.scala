package com.andbutso.ajatella

trait Grapheme {
  def letter: Char

  def isConsonant = {
    isInstanceOf[Consonant]
  }

  def isVowel = {
    isInstanceOf[Vowel]
  }

  def isLetter = {
    isConsonant || isVowel
  }

  def isNonLetter = {
    !isLetter
  }

  override def toString = {
    letter.toString
  }

  def unary_! = {
    Alternate.Not(this)
  }

  import Alternate.Identity.graphemeToIdentityAlternate

  def ∙(otherGrapheme: Grapheme) = {
    GraphemeMatcher(this) ∙ otherGrapheme
  }

  def ∙(alternate: Alternate) = {
    GraphemeMatcher(this) ∙ alternate
  }

  def |(otherGrapheme: Grapheme) = {
    GraphemeMatcher(this) | otherGrapheme
  }

  def |(alternate: Alternate) = {
    GraphemeMatcher(this) | alternate
  }
}

case class NonLetter(letter: Char) extends Grapheme

//case class Grapheme2(val letter: Either[Char, Alternate]) extends AnyVal {
//  import Lexeme.stringToLexeme
//
//  def interpolate(lexeme: Lexeme) = {
//    letter match {
//      case Left(string) =>
//        string
//      case Right(alternate) =>
//        alternate match {
//          case vha: VowelHarmonyAlternate =>
//            vha(lexeme.vowelType)
//          case Alternate.V =>
//            lexeme.letters.filter { _.isVowel }.last
//          case _ =>
//            alternate.toString
//        }
//    }
//  }
//
//    override def toString = {
//      letter match {
//        case Left(char) =>
//          char.toString
//        case Right(alternate) =>
//          alternate match {
//            case Alternate.A => "A"
//            case Alternate.U => "U"
//            case Alternate.O => "O"
//            case Alternate.V => "V"
//          }
//      }
//    }
//}

object Grapheme {
  def fromChar(character: Char): Grapheme = {
    if (Vowel.isVowel(character)) {
      Vowel(character)
    } else if (Consonant.isConsonant(character)) {
      Consonant(character)
    } else {
      NonLetter(character)
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