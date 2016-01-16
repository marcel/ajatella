package com.andbutso.ajatella

object Decompounder {
  def apply(word: String) = {
    val decompounder = new Decompounder(word)
    decompounder.traverse
  }
}

// A different algorithm is described in http://members.unine.ch/jacques.savoy/Papers/CLEF2003MonoWP.pdf
class Decompounder(word: String) {
  val matches = {
    val subs = 2.to(word.size - 2).flatMap { n => word.sliding(n) }
    subs.filter { sub => WordList.entries(sub).isDefined || WordList.list.contains(sub) }
  }

  val matchesByStartingIndex = {
    val lookup = collection.mutable.Map[Int, Set[String]]()
    matches foreach { subString =>
      val startingIndex = word.indexOf(subString)
      val substringsAtIndex = lookup.getOrElseUpdate(startingIndex, Set.empty)
      lookup(startingIndex) = substringsAtIndex + subString
    }

    lookup
  }

  def traverse = {
    extend(
      matchesByStartingIndex.getOrElse(0, Seq(word)).toSeq map { prefix => Seq(prefix)}
    ).filter { result =>
      result.mkString.size == word.size
    }.sortBy {
      _.size
    }.headOption.getOrElse(Seq(word))
  }

  def extend(parts: Seq[Seq[String]]): Seq[Seq[String]] = {
    parts flatMap { part =>
      matchesByStartingIndex.get(part.mkString.size) match {
        case None =>
          parts
        case Some(more) =>
          extend(more.toSeq map { another => part :+ another})
      }
    }
  }
}
