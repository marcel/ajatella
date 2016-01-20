package com.andbutso.ajatella

class MorphologySpec extends ParentSpec {
  "Morphology" should {
    "detect matches" in {
      val matches = Seq("talosta", "tytöstä", "maasta", "kaikesta", "kädestä", "perheestä", "päätöksestä", "tehtaasta", "totuudesta", "talostaan")
      val morph = Morphology.Form.Nouns(206)

      matches foreach { m =>
        if (morph.matches(m)) {
          ok
        } else {
          failure(s"'$m' did not match $morph")
        }
      }

      ok
    }
  }
}
