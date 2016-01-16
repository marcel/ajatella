package com.andbutso.ajatella

class DecompounderSpec extends ParentSpec {
  "Decompounder" should {
    "split words into largest possible sub words to form complete compound word" in {

      val expectations = Map(
        "sokerisirotin"         -> Seq("sokeri", "sirotin"),
        "sokeritautipotilas"    -> Seq("sokeritauti", "potilas"),
        "sokeriaineenvaihdunta" -> Seq("sokeri", "aineenvaihdunta")

      )

      expectations foreach { case (word, expectation) =>
        Decompounder(word) mustEqual expectation
      }

      ok
    }
  }
}
