package com.andbutso.ajatella

object Letters {
  def apply(letters: Any*): Letters = {
    new Letters(
      letters.collect {
        case string: String => Left(string)
        case alt: Alternate => Right(alt)
        case _ => throw new IllegalArgumentException("Letter must be String or Alternate")
      }
    )
  }
}

class Letters(val letters: Seq[Either[String, Alternate]])