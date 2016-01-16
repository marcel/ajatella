package com.andbutso

package object ajatella {
  val Encoding = "ISO8859_4" // Northern European

  import org.kiama.output.PrettyPrinter._

  def pp(o: Any) = {
    println(pretty(any(o), w = 1))
  }
}
