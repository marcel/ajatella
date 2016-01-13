package com.andbutso.ajatella

object GraphemeMatcher {
  def apply(alternate: Alternate) = {
    new GraphemeMatcher(Some(alternate))
  }

  implicit def graphemeToGraphemeMatcher(grapheme: Grapheme): GraphemeMatcher = {
    GraphemeMatcher(Alternate.Identity(grapheme.letter.toString))
  }

  implicit def alternateToGraphemeMatcher(alternate: Alternate): GraphemeMatcher = {
    GraphemeMatcher(alternate)
  }
}

class GraphemeMatcher(initialAlternate: Option[Alternate] = None) {
  import Lexeme.stringToLexeme

  val indexes = new collection.mutable.ArrayBuffer[Alternate]() ++ initialAlternate
  def size = indexes.size

  def apply(lexeme: Lexeme) = {
    indexIn(lexeme).map { startingIndex =>
      val letters = lexeme.string.slice(startingIndex, startingIndex + size).letters

      letters.zipWithIndex.collect {
        case (g, index) if indexes(index).isDefinedAt(g) =>
          val alternate = indexes(index)
          alternate(g.toString)
      }
    }
  }

  def transform(
    lexeme: Lexeme
  )(
    pf: PartialFunction[Array[Lexeme], Array[Lexeme]]
  ): Option[Lexeme] = {
    val interpolated = apply(lexeme).getOrElse(Array.empty)
    if (pf.isDefinedAt(interpolated)) {
      val replacements = pf(interpolated).mkString
      Some(lexeme.string.patch(indexIn(lexeme).get, replacements, size))
    } else {
      None
    }
  }

  def interpolate(lexeme: Lexeme) = {
    indexes.map { a => a(lexeme) } mkString
  }

  def ∙(otherAlternate: Alternate) = {
    indexes.append(otherAlternate)
    this
  }

  def ∙(otherMatcher: GraphemeMatcher) = {
    indexes.appendAll(otherMatcher.indexes)
    this
  }

  def |(a: Alternate) = {
    val lastElement = indexes.last
    indexes.update(indexes.size - 1, lastElement or a)
    this
  }

//  def |(otherMatcher: GraphemeMatcher) = {
//
//  }

  def indexIn(lexeme: Lexeme) = {
    lexeme.string.sliding(size).zipWithIndex.foldRight(None: Option[Int]) {
      case ((subString, atIndex), matchingIndex) =>
        matchingIndex match {
          case Some(_) => matchingIndex
          case _ =>
            if (matches(subString)) {
              Some(atIndex)
            } else {
              None
            }
        }
    }
  }

  def matches(string: String) = {
    val letters = string.toCharArray.take(indexes.size)

    letters.zipWithIndex.forall { case ((character, atIndex)) =>
      val alternate = indexes(atIndex)
      alternate.isDefinedAt(character)
    }
  }
}

// TODO fix naming/taxonomy. Aside from the vowel harmony alternates the rest
// aren't really alternates as they are letter sequence sets which dictate the type
// and sequence of one or more letters
// Should perhaps be called GraphemeMatcher or something
trait Alternate {
  import Lexeme.stringToLexeme

  def isDefinedAt(o: Any) = this.equals(o)

  // Should return the interpolated version based on the lexeme
  def apply(lexeme: Lexeme): Lexeme = {
    lexeme.toString
  }

//  def interpolate(lexeme: Lexeme): Lexeme

//  def ∙(otherAlternate: Alternate): IndexedAlternate = {
//    this andThen otherAlternate
//  }
  def ∙(otherAlternate: Alternate) = {
    GraphemeMatcher(this) ∙ otherAlternate
  }

//  def andThen(otherAlternate: Alternate): IndexedAlternate = {
//    val self = this
//
//    new IndexedAlternate(1, self) {
//      override def isDefinedAt(o: Any) = {
//        IndexedAlternate.isDefinedAtIndex(0, self, o) &&
//          IndexedAlternate.isDefinedAtIndex(1, otherAlternate, o)
//      }
//    }
//  }

  def |(a: Alternate) = {
    GraphemeMatcher(this or a)
  }

  def or(otherAlternate: Alternate) = {
    Alternate.Or(this, otherAlternate)
  }

//  def or(otherAlternate: Alternate): Alternate = {
//    val self = this
//
//    new Alternate {
//      override final def apply(lexeme: Lexeme) = {
//        if (self.isDefinedAt(lexeme)) {
//          self(lexeme)
//        } else if (otherAlternate.isDefinedAt(lexeme)) {
//          otherAlternate(lexeme)
//        } else {
//          lexeme
//        }
//      }
//
//      override def isDefinedAt(o: Any) = {
//        self.isDefinedAt(o) || otherAlternate.isDefinedAt(o)
//      }
//    }
//  }
}

//object IndexedAlternate {
//  def isDefinedAtIndex(index: Int, alternate: Alternate, o: Any) = {
//    println(s"checking index $index")
//    o match {
//      case c: Char =>
//        alternate.isDefinedAt(c)
//      case s: String if s.isDefinedAt(index) =>
//        alternate.isDefinedAt(s.charAt(index))
//      case g: Grapheme =>
//        alternate.isDefinedAt(g.letter)
//      case _ =>
//        alternate.hashCode().equals(o.hashCode())
//    }
//  }
//}

//case class IndexedAlternate(nextIndex: Int, underlying: Alternate) extends Alternate {
//  def size = nextIndex + 1

//  override def andThen(otherAlternate: Alternate): IndexedAlternate = {
//    val self = this
//
//    // TODO This approach of increasing nextIndex each time we compose with another IndexedAlternate
//    // doesn't work because the IndexedAlternate could be another composed IndexAlternate which then
//    // gets wrapped in a composed alternate without updating its underlying indexes
//    new IndexedAlternate(nextIndex + 1, self) {
//      override def isDefinedAt(o: Any) = {
//        self.isDefinedAt(o) &&
//          IndexedAlternate.isDefinedAtIndex(nextIndex, otherAlternate, o)
//      }
//    }
//  }

//  override def |(otherAlternate: Alternate) = or(otherAlternate)

//  override def or(otherAlternate: Alternate): IndexedAlternate = {
//    val self = this
//
//    new IndexedAlternate(nextIndex, self) {
//      override final def apply(lexeme: Lexeme) = {
//        if (self.isDefinedAt(lexeme)) {
//          self(lexeme)
//        } else if (otherAlternate.isDefinedAt(lexeme)) {
//          otherAlternate(lexeme)
//        } else {
//          lexeme
//        }
//      }
//
//      override def isDefinedAt(o: Any) = {
//        self.isDefinedAt(o) || otherAlternate.isDefinedAt(o)
//      }
//    }
//  }
//}

object Alternate {
  import Grapheme.charToGrapheme

  case object Vowel extends Alternate {
    override def equals(o: Any) = isDefinedAt(o)

    override def isDefinedAt(o: Any): Boolean = {
      o match {
        case c: Char =>
          c.isVowel
        case s: String =>
          s.size == 1 && s.toCharArray.head.isVowel
        case v: com.andbutso.ajatella.Vowel =>
          true
        case _ =>
          hashCode().equals(o.hashCode())
      }
    }
  }

  case object Consonant extends Alternate {
    override def equals(o: Any) = isDefinedAt(o)

    override def isDefinedAt(o: Any): Boolean = {
      o match {
        case c: Char =>
          c.isConsonant
        case s: String =>
          s.size == 1 && s.toCharArray.head.isConsonant
        case c: com.andbutso.ajatella.Consonant =>
          true
        case _ =>
          hashCode().equals(o.hashCode())
      }
    }
  }

  case class Not(grapheme: Grapheme) extends Alternate {
    override def equals(o: Any): Boolean = {
      o match {
        case c: Char => !grapheme.equals(o)
        case s: String => !grapheme.equals(o)
        case g: Grapheme => !grapheme.equals(o)
        case _ => true
      }
    }
  }

  case class Or(lhs: Alternate, rhs: Alternate) extends Alternate {
    override final def apply(lexeme: Lexeme) = {
      if (lhs.isDefinedAt(lexeme)) {
        lhs(lexeme)
      } else if (rhs.isDefinedAt(lexeme)) {
        rhs(lexeme)
      } else {
        lexeme
      }
    }

    override def isDefinedAt(o: Any) = {
      lhs.isDefinedAt(o) || rhs.isDefinedAt(o)
    }
  }

  case class Identity(string: String) extends Alternate {
    override def apply(lexeme: Lexeme): Lexeme = {
      string
    }

    override def isDefinedAt(o: Any) = string.equals(o.toString)
  }

  object Identity {
    implicit def charToIdentityAlernate(char: Char) = {
      Identity(char.toString)
    }

    implicit def stringToIdentityAlternate(string: String) = {
      Identity(string)
    }

    implicit def graphemeToIdentityAlternate(grapheme: Grapheme) = {
      Identity(grapheme.letter.toString)
    }
  }

  // TODO Constrain to only Graphame.isLetter
  case object AnyLetter extends Alternate {
    override def apply(lexeme: Lexeme): Lexeme = {
      lexeme
    }

    override def equals(o: Any) = true

    override def isDefinedAt(o: Any) = true

    override def toString = "❋"
  }

  val L = AnyLetter
  val V = Vowel
  val C = Consonant
  val A = VowelHarmonyAlternate('a', 'ä')
  val O = VowelHarmonyAlternate('o', 'ö')
  val U = VowelHarmonyAlternate('u', 'y')
}

case class VowelHarmonyAlternate(back: Vowel, front: Vowel) extends Alternate {
  import Lexeme.stringToLexeme

  def apply(vowelType: Vowel.Type): Lexeme = {
    vowelType match {
      case Vowel.Back  => back.letter.toString
      case Vowel.Front => front.letter.toString
      case Vowel.Neutral =>
        throw new IllegalArgumentException("Vowel type must be Front or Back, was Neutral")
    }
  }

  override def apply(lexeme: Lexeme): Lexeme = {
    apply(lexeme.vowelType)
  }

  override final def isDefinedAt(o: Any): Boolean = {
    o match {
      case c: Char =>
        val v = Vowel(c)
        v == back || v == front
      case Vowel(v) =>
        isDefinedAt(v)
      case vha: VowelHarmonyAlternate =>
        isDefinedAt(front) || isDefinedAt(back)
      case _ =>
        false
    }
  }
}