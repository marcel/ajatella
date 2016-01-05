package com.andbutso.ajatella

import scala.annotation.tailrec

case class Consonant(letter: Char) extends Grapheme {
  @tailrec
  override final def equals(o: Any): Boolean = {
    o match {
      case consonant: Consonant =>
        equals(consonant.letter)
      case string: String =>
        letter.toString.toUpperCase.equals(string.toUpperCase)
      case char: Char =>
        letter.toUpper.equals(char.toUpper)
      case _ =>
        false
    }
  }
}
