package com.andbutso.ajatella

object Morphology {
  import Alternate._
  import GraphemeMatcher._
  import Case._
  import Possessive._
  import Number._
  import Consonants._
  import Vowels._
  import Alternate.Identity._

  def identify(s: String) = {
    val results = Morphology.Form.Nouns.values.collect { case n if n.matches(s) => (n, n.suffixMatch(s)) }

    val results2 = results.map { r => (r._1, r._2.get, r._2.get.length) }.toSeq.sortBy { case (a,b,c) => -c }
    println(results2.map { case (a,b,c) => (a.identifier, b, c)})
    results2.head._1
  }

  trait Tag

  trait Form extends TextualDescription {
    def morphology: Morphology
  }

  object Noun {
    def apply(
      stemEnding: GraphemeMatcher,
      numberEnding: GraphemeMatcher,
      nounCase: Case,
      caseEnding: GraphemeMatcher,
      possessive: Possessive.EndingSet
    ): Noun = {
      Noun(stemEnding, Plural, Some(numberEnding), nounCase, caseEnding, Some(possessive))
    }

    def apply(
      stemEnding: GraphemeMatcher,
      numberEnding: GraphemeMatcher,
      nounCase: Case,
      caseEnding: GraphemeMatcher
    ): Noun = {
      Noun(stemEnding, Plural, Some(numberEnding), nounCase, caseEnding)
    }

    def apply(
      stemEnding: GraphemeMatcher,
      nounCase: Case,
      caseEnding: GraphemeMatcher,
      possessive: Possessive.EndingSet
    ): Noun = {
      Noun(stemEnding, Singular, None, nounCase, caseEnding, Some(possessive))
    }

    def apply(
      stemEnding: GraphemeMatcher,
      nounCase: Case,
      caseEnding: GraphemeMatcher
    ): Noun = {
      Noun(stemEnding, Singular, None, nounCase, caseEnding, None)
    }
  }

  case class Noun(
    stemEnding: GraphemeMatcher,
    number: Number,
    numberEnding: Option[GraphemeMatcher],
    nounCase: Case,
    caseEnding: GraphemeMatcher,
    possessive: Option[Possessive.EndingSet] = None
  ) extends Form {
    def toText = {
      s"Noun ${number.toText} ${nounCase.name} (-${stemEnding.toText}.${caseEnding.toText})"
    }

    def identifier = {
      Form.NounToIdentifier(this)
    }

    def size = {
      stemEnding.size + numberEnding.map { _.size }.getOrElse(0) + caseEnding.size
    }

    def suffixMatch(lexeme: Lexeme) = {
      Clitic.All.collectFirst {
        case clitic if lexeme.endsWith(clitic.ending) =>
          println("matched clitic: " + clitic.ending.toText)
          clitic.ending.dropFrom(lexeme)
     }.getOrElse(Some(lexeme)).flatMap { cliticDropped =>
        println("clitic dropped: " + cliticDropped)
        possessive.flatMap { possessives =>
          possessives.collectFirst {
            case possessive if cliticDropped.endsWith(possessive.ending) =>
              println("matched possessive: " + possessive.ending.toText)
              possessive.ending.dropFrom(cliticDropped)
          }
        }.getOrElse(Some(cliticDropped)).flatMap { possessiveDropped =>
          println("possessive dropped: " + possessiveDropped)
          println("matching against case ending: " + caseEnding.toText)
          caseEnding.dropFrom(possessiveDropped)
        }.flatMap { caseEndingDropped =>
          println("case ending dropped: " + caseEndingDropped)
          (numberEnding match {
            case Some(ending) if caseEndingDropped.endsWith(ending) =>
              println("matching against number ending: " + ending.toText)
//              ending.dropFrom(caseEndingDropped)
              Some(caseEndingDropped)
            case Some(nonMatchingEnding) =>
              println("did not match number ending: " + nonMatchingEnding.toText)
              None
            case _ => Some(caseEndingDropped)
          }).flatMap { numberEndingDropped =>
            println("matching against stem ending: " + stemEnding.toText)
            if (numberEndingDropped.endsWith(stemEnding)) {
              Some(lexeme.substring(numberEndingDropped.length - stemEnding.size, lexeme.length))
            } else {
              None
            }
          }
        }
      }
    }

    def matches(lexeme: Lexeme) = {
      suffixMatch(lexeme).isDefined
    }

    def morphology = {
      Morphology(
        PartOfSpeech.Noun,
        stemEnding,
        //      tenseOrMoodOrNumber = Some(number)
        caseOrPerson = Some(Left(caseEnding)),
        possessive = possessive
      )
    }
  }

//  val PossibleStructures = Map(
//    // talosta(an), tytöstä, maasta, kaikesta, kädestä, perheestä, päätöksestä, tehtaasta, totuudesta
//    206 -> Morphology("N", V, None, None, None, Some(Left(Case.Elative.stA)), Some(Possessive.Two))
//  )

  object Form {
    // TODO The POS column is either required to be present or if specified in parens, e.g. (POS-2) then it is optional
    // This current definition assumes that if it is present then it is required.
    val Nouns = Map(
        1 -> Noun(V|C,          Nominative,  L           ), // TODO Not L, should be absent
        2 -> Noun(V,            Nominative,  L,       One), // TODO Not L, should be absent
        3 -> Noun(V,            Genitive,    L,       One), // TODO Not L, should be absent
        4 -> Noun(V,         i, Nominative,  L,       One), // TODO Not Plural.i & not L, both should be absent
       40 -> Noun(C∙V,          Partative,   A,       Two),
       44 -> Noun(C,         i, Partative,   A,       Two),
       46 -> Noun(C∙V,       j, Partative,   A,       Two),
       55 -> Noun(V∙V,       i, Genitive,    d∙e,     One),
       57 -> Noun(V∙V,       i, Genitive,    d∙e∙n       ),
       58 -> Noun(V,         i, Accusative,  d∙ä∙t       ),
       59 -> Noun(C∙i,       i, Genitive,    e,       One), // TODO Not Plural.i, the column is blank
       61 -> Noun(C,         i, Genitive,    e,       One),
       62 -> Noun(C∙V,       i, Genitive,    e,       One),
       65 -> Noun(C∙i,       i, Genitive,    e∙n         ), // TODO Not Plural.i, the column is blank
       69 -> Noun(C,         i, Genitive,    e∙n         ),
       71 -> Noun(C∙V,       j, Genitive,    e∙n         ),
       73 -> Noun(V∙V,       i, Illative,    h∙i,     One),
       75 -> Noun(V∙V,       i, Illative,    h∙i∙n       ),
       76 -> Noun(V∙V,          Illative,    h∙V,     One),
       77 -> Noun(V∙V,          Illative,    h∙V∙n       ),
       79 -> Noun(C,         i, Illative,    i,       One),
       84 -> Noun(C,         i, Illative,    i∙n         ),
       87 -> Noun(V,            Translative, k∙s∙e,   Two),
       89 -> Noun(V|C,       i, Translative, k∙s∙e,   Two),
       99 -> Noun(V,            Translative, k∙s∙i       ),
      105 -> Noun(V|C,       i, Translative, k∙s∙i       ),
      111 -> Noun(V,            Adessive,    l∙l∙A,   Two),
      117 -> Noun(V|C,       i, Adessive,    l∙l∙A,   Two),
      124 -> Noun(V,            Allative,    l∙l∙e,   Two),
      130 -> Noun(V|C,       i, Allative,    l∙l∙e,   Two),
      136 -> Noun(V,            Ablative,    l∙t∙A,   Two),
      142 -> Noun(V|C,       i, Ablative,    l∙t∙A,   Two),
      153 -> Noun(V,            Genitive,    n           ),
      167 -> Noun(V,            Essive,      n∙A,     Two),
      173 -> Noun(V|C,       i, Essive,      n∙A,     Two),
      177 -> Noun(V∙V,          Illative,    s∙e∙e,   One),
      179 -> Noun(V∙V,          Illative,    s∙e∙e∙n     ),
      180 -> Noun(V∙V,       i, Illative,    s∙i∙i,   One),
      182 -> Noun(V∙V,       i, Illative,    s∙i∙i∙n     ),
      188 -> Noun(V,            Inessive,    s∙s∙A,   Two),
      194 -> Noun(V|C,       i, Inessive,    s∙s∙A,   Two),
      206 -> Noun(V,            Elative,     s∙t∙A,   Two),
      212 -> Noun(V|C,       i, Elative,     s∙t∙A,   Two),
      221 -> Noun(V,         t, Nominative,  L           ),
      222 -> Noun(V,            Accusative,  t           ),
      226 -> Noun(V∙V,          Partative,   t∙A         ),
      227 -> Noun(l|n|r|s|t,    Partative,   t∙A,     Two),
      229 -> Noun(V∙V,       i, Partative,   t∙A,     Two),
      230 -> Noun(l|n|r|s,      Genitive,    t∙e,     One), // TODO This is actually a plural, not singlar as the default is without a number specified
      231 -> Noun(l|n|r|s,      Genitive,    t∙e∙n       ), // Ditto
      232 -> Noun(C∙e,          Partative,   t∙t∙A       ),
      233 -> Noun(V,            Abessive,    t∙t∙A,   Two),
      234 -> Noun(V|C,       i, Abessive,    t∙t∙A,   Two),
      238 -> Noun(V∙V,       i, Genitive,    t∙t∙e,   One),
      241 -> Noun(V∙V,       i, Genitive,    t∙t∙e∙n     ),
      244 -> Noun(C∙V,          Illative,    V           ),
      248 -> Noun(C∙V,          Illative,    V∙n         )
    )

    val NounToIdentifier = Nouns.map { _.swap }
  }
}

// TODO Set correct types for each attribute
// TODO Determine if it makes more sense to create subclasses
// for the 3 possible parts of speech
case class Morphology(
  partOfSpeech: PartOfSpeech, // N, V or A
  stemEnding: GraphemeMatcher, // V/C, V, VV, etc
  voice: Option[String] = None, // Active or Passive or N/A
  verbForm: Option[String] = None, // Participle or Infinitive or N/A
  tenseOrMoodOrNumber: Option[String] = None, // how to model 3 possible values? EitherOr :-o
  caseOrPerson: Option[Either[GraphemeMatcher, Person]], //
  possessive: Option[Possessive.EndingSet]
) {
  def matches(lexeme: Lexeme) = {
    // TODO Check for clitics
    // TODO Check for plurals

//    caseOrPerson flatMap {
//      case Left(suffix) if lexeme.endsWith(suffix) =>
//        lexeme.drop(suffix(lexeme))
//      case _ => None
//    } map { truncated =>
//      stemEnding == truncated.letters.last
//    } getOrElse(false)

  }
}

// Steps:
// 1. Check whether there is a clitic. If so, detach it.
// 2. Check whether there is a possessive. If there appears to be one detach it and
// check the remainder in the case/person column.
class MorphologyDetector
