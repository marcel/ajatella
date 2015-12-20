package com.andbutso.ajatella

class VowelHarmonyAlternateSpec extends ParentSpec {
  "VowelHarmonyAlternate" should {
    import Alternate.{A, O, U}

    "equals" in {
      val expectations = Map(
        A -> Set('a', 'ä', 'A', 'Ä'),
        O -> Set('o', 'ö', 'O', 'Ö'),
        U -> Set('u', 'y', 'U', 'Y')
      )

      expectations foreach { case (alternate, matches) =>
        matches foreach { expectedMatch =>
          alternate mustEqual expectedMatch
        }

        val nonMatches = expectations.values.flatten.toSet -- matches

        nonMatches foreach { expectedNonMatch =>
          alternate mustNotEqual expectedNonMatch
        }
      }

      ok
    }
  }
}
