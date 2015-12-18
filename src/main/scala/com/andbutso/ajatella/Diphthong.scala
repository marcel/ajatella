package com.andbutso.ajatella

// A diphthong is a combination of two vowels in the same syllable.
sealed case class Diphthong(letters: String) {
  require(letters.size == 2)

  def vowels = {
    letters.toCharArray.map { character =>
      Vowel(character)
    }
  }

  override final def equals(o: Any) = {
    o match {
      case diphthong: Diphthong =>
        hashCode.equals(diphthong.hashCode)
      case _ =>
        false
    }
  }

  override def hashCode = vowels.map { _.hashCode }.mkString.hashCode()
}

object Diphthong {         // e.g.
  val ai = Diphthong("ai") // kai-vo    (a well)
  val au = Diphthong("au") // au-to     (car)
  val ei = Diphthong("ei") // kei-no    (a means)
  val eu = Diphthong("eu") // neu-vo    (piece of advice)
  val ie = Diphthong("ie") // sie-vä    (pretty)
  val iu = Diphthong("iu") // liu-ku-a  (to slide)
  val oi = Diphthong("oi") // koit-taa  (to try)
  val ou = Diphthong("ou") // hou-sut   (pants)
  val ui = Diphthong("ui") // ui-da     (to swim)
  val uo = Diphthong("uo") // Puo-la    (Poland)
  val yi = Diphthong("yi") // myin      (I sold)
  val yö = Diphthong("yö") // myö-hään  (late)
  val äi = Diphthong("äi") // äi-ti     (mother)
  val äy = Diphthong("äy") // käy-rä    (crooked)
  val öi = Diphthong("öi") // öi-sin    (at night)
  val öy = Diphthong("öy") // möy-kä-tä (to make noise)

  val All = Set(
    ei,
    äi,
    ui,
    ai,
    oi,
    öi,
    yi,
    au,
    ou,
    eu,
    iu,
    äy,
    öy,
    ie,
    yö,
    uo
  )

  def isDiphthong(string: String) = {
    string.size == 2 && All.contains(Diphthong(string))
  }
}