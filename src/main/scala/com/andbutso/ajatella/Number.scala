package com.andbutso.ajatella

trait Number extends TextualDescription

object Number {
  case object Singular extends Number {
    def toText = "singular"
  }

  case object Plural extends Number {
    def toText = "plural"
  }

//  object Plural {
//    case object j extends Plural
//    case object i extends Plural
//    case object t extends Plural
//  }
//  case object Plural extends Number {
//    def :+:(lexeme: Lexeme): Lexeme = {
//      lexeme.string + "i"
//    }
//    def alternates = ('t' /* Nominative */ , 'i' /* All other cases */ )
//  }
}