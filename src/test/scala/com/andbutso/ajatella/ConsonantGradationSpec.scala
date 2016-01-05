package com.andbutso.ajatella

class ConsonantGradationSpec extends ParentSpec {
  "ConsonantGradation" should {
    import Case._
    val cg = new ConsonantGradation

    "for quantitative gradation" in {
      "go from strong to weak when inflecting" in {
//        cg("kaappi", Inessive) mustEqual "kaapissa"
        ok
      }

      "go from weak to strong when reverting inflected form to basic form" in {
        ok
      }

    }

    "go from strong to weak" in {
      val expectations = Map(
        "kaappi"   -> "kaapin",    // pp  : p   (#1)
        "tyttö"    -> "tytön",     // tt  : t   (#2)
        "kukka"    -> "kukan",     // kk  : k   (#3)
        "tapa"     -> "tavan",     // p   : v   (#4)
        "katu"     -> "kadun",     // t   : d   (#5)
        "lehti"    -> "lehden",    // ht  : hd  (#6)
        "hakea"    -> "haen",      // k   : 0   (#7)
        "kampa"    -> "kamman",    // mp  : mm  (#8)
        "ranta"    -> "rannan",    // nt  : nn  (#9)
        "kulta"    -> "kullan",    // lt  : ll  (#10)
        "parta"    -> "parran",    // rt  : rr  (#11)
        "Helsinki" -> "Helsingin", // nk  : ng  (#12)
        "sulkea"   -> "suljen",    // lke : lje (#13)
        "särkeä"   -> "särjen",    // rke : rje (#14)
        "rohkenen" -> "rohjeta",   // hke : hje (#15)
        "luku"     -> "luvun",     // k   : v   (#16)
        "kyky"     -> "kyvyn"
      )

      ok
    }
  }
}
