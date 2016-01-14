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
    subs.filter { sub => WordList.entries(sub).isDefined }
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

  def startingIndexes = {
    matchesByStartingIndex.keys.toSeq.sorted
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

class FirstAttemptDecompounder {
  def apply(word: String) = {
    val subs = 2.to(word.size - 2).flatMap { n => word.sliding(n) }
    val matches = subs.collect {
      case sub if WordList.entries(sub).isDefined =>
        sub -> word.indexOf(sub)
    }

    val subsWithResults = matches.map { _._1 }

    val subsWithResultsSorted = subsWithResults.sortBy { sub => word.indexOf(sub) }

    val keepLongests = subsWithResults.filterNot { sub =>
      subsWithResults.exists { otherSub => sub != otherSub && otherSub.contains(sub) }
    }

    def rangeInWord(sub: String) = {
      val subIndex = word.indexOf(sub)
      Range(subIndex, subIndex + sub.size)
    }

    val sorted = keepLongests.sortBy { sub => word.indexOf(sub) }

    sorted.tail.foldLeft(Seq(sorted.head)) { case (subsToKeep, sub) =>
      if (rangeInWord(subsToKeep.last).intersect(rangeInWord(sub)).isEmpty) {
        subsToKeep :+ sub
      } else {
        subsToKeep
      }
    }
  }
}
