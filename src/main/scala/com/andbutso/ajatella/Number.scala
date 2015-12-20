package com.andbutso.ajatella

trait Number extends Alternate
object Number {
  case object Singular extends Number
  case object Plural extends Number {
    def alternates = ('t' /* Nominative */ , 'i' /* All other cases */ )
  }
}