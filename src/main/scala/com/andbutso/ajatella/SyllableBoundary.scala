package com.andbutso.ajatella

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

// A syllable boundary occurs before every sequence of a single
// consonant followed by a vowel.
//
//   e.g.
//
//    si-ka-la           (pig sty)
//    ko-men-to-kes-kus  (command center)
//    raa-mat-tu         (bible)
//    sai-raa-la         (hospital)
//    hel-sin-ki-läi-nen (Helsinkier)
//    a-len-nus-myyn-ti  (sale)
//    vah-ti-mes-ta-ri   (janitor)
//    kat-ti-la          (pot)
//
// There is also a syllable boundary between vowels that do not
// form a Diphthong as shown in the following examples:
//
//    ka-pe-a            (narrow)
//    ko-et-taa          (to try)
//    pi-an              (soon)
//    lu-e-tel-la        (to enumerate)
//    sa-no-a            (to say)
//    vi-re-ä            (vivacious)
//    as-tu-a            (to step)
//    kää-ri-ä           (to roll)
//    ra-e               (hailstone)
object SyllableBoundary {
  def split(word: String) = {
    val syllables  = new mutable.ArrayBuffer[ArrayBuffer[Char]]()
    val characters = word.toCharArray.toIndexedSeq

    characters.zipWithIndex.foreach { case (character, index) =>
      if (SyllableBoundaryDetector(characters, index).isBoundary) {
        syllables += ArrayBuffer(character)
      } else {
        syllables.last += character
      }
    }

    syllables map { letters => letters.mkString("") } toSeq
  }
}

case class SyllableBoundaryDetector(characters: IndexedSeq[Char], index: Int) {
  val character = characters(index)

  def isBoundary = {
    index == 0 || isConsonantFollowedByVowel || isNonDiphthongUnidenticalVowelPair
  }

  private[this] def isConsonantFollowedByVowel = {
    nextCharacter.exists { nextCharacter =>
      !Vowel.isVowel(character) && Vowel.isVowel(nextCharacter)
    }
  }

  private[this] def isNonDiphthongUnidenticalVowelPair = {
    previousCharacter.exists { previousCharacter =>
      Vowel.isVowel(character) && Vowel.isVowel(previousCharacter) &&
        !Diphthong.isDiphthong(s"$previousCharacter$character") &&
        Vowel(character) != Vowel(previousCharacter)
    }
  }

  private[this] def previousCharacter = {
    if (index != 0) {
      Some(characters(index - 1))
    } else {
      None
    }
  }

  private[this] def nextCharacter = {
    if (index != characters.size - 1) {
      Some(characters(index + 1))
    } else {
      None
    }
  }

}
