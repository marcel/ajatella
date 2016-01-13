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

object Consonants {
  val p = Consonant('p')
  val t = Consonant('t')
  val k = Consonant('k')
  val d = Consonant('d')
  val g = Consonant('g')
  val s = Consonant('s')
  val h = Consonant('h')
  val v = Consonant('v')
  val j = Consonant('j')
  val l = Consonant('l')
  val r = Consonant('r')
  val m = Consonant('m')
  val n = Consonant('n')

  val b = Consonant('b')
  val c = Consonant('c')
  val f = Consonant('f')
  val w = Consonant('w')
  val x = Consonant('x')
  val z = Consonant('z')
}

object Consonant {
  import Consonants._

  val Native = Set(
    p,
    t,
    k,
    d,
    g,
    s,
    h,
    v,
    j,
    l,
    r,
    m,
    n
  )

  val FromLoanWords = Set(
    b,
    c,
    f,
    w,
    x,
    z
  )

  val All = Native ++ FromLoanWords

  def isConsonant(character: Char) = {
    All.contains(Consonant(character.toLower))
  }
}
