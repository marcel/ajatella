package com.andbutso.ajatella

class MorphologySpec extends ParentSpec {
  "Morphology" should {
    "detect matches" in {
      val matches = Seq("talosta", "tytöstä", "maasta", "kaikesta", "kädestä", "perheestä", "päätöksestä", "tehtaasta", "totuudesta", "talostaan")
      val morph = Morphology.PossibleStructures.values.head

      matches foreach { m =>
        if (!morph.matches(m)) {
          failure(s"'$m' did not match $morph")
        } else {
          ok
        }
      }

      ok
    }
  }
}
