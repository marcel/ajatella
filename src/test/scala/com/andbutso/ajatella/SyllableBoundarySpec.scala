package com.andbutso.ajatella

class SyllableBoundarySpec extends ParentSpec {
  "SyllableBoundary" should {
    "split on syllable boundaries" in {
      val expectations = Seq(
        "ka-la",
        "jo-kai-nen",
        "kui-ten-kin",
        "sit-ten",
        "päi-vä",
        "al-kaa",
        "pur-kis-sa",
        "purk-kiin",
        "An-tin",
        "An-til-le",
        "Hel-sin-gis-sä-kin",
        "puis-sa",
        "nais-ta-kin",
        "si-ka-la",
        "ko-men-to-kes-kus",
        "raa-mat-tu",
        "sai-raa-la",
        "hel-sin-ki-läi-nen",
        "a-len-nus-myyn-ti",
        "vah-ti-mes-ta-ri",
        "kat-ti-la",
        "ka-pe-a",
        "ko-et-taa",
        "pi-an",
        "lu-e-tel-la",
        "sa-no-a",
        "vi-re-ä",
        "as-tu-a",
        "kää-ri-ä",
        "ra-e",
        "myin",
        "myö-hään",
        "liu-ku-a",
        "koit-taa",
        "möy-kä-tä",
        "pork-ka-na"
      ) map { word =>
        word.replaceAllLiterally("-", "") -> word.split('-').toSeq
      } toMap

      expectations foreach { case (word, expectedBoundaries) =>
        SyllableBoundary.split(word) mustEqual expectedBoundaries
      }

      ok
    }
  }
}
