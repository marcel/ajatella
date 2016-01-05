package com.andbutso.ajatella

trait Number extends Alternate
object Number {
  case object Singular extends Number
  trait Plural extends Number
  object Plural {
    case object i extends Plural
    case object t extends Plural
  }
//  case object Plural extends Number {
//    def :+:(lexeme: Lexeme): Lexeme = {
//      lexeme.string + "i"
//    }
//    def alternates = ('t' /* Nominative */ , 'i' /* All other cases */ )
//  }
}