package com.andbutso.ajatella

import com.andbutso.ajatella.PrefixSearchTree.{Branch, Children}

import scala.annotation.tailrec
import scala.collection.mutable

object PrefixSearchTree {
  type Children = mutable.Map[Char, Node]
  val EmptyChildren = mutable.Map[Char, Node]()
  val TerminatedChild = EmptyChildren + (Leaf.letter -> Leaf)

  case class Path(prefix: String, location: Node) {
    def words = {
      def descend(
        nodes: List[Node],
        letters: List[Char],
        words: Set[String]
      ): Set[String] = {
        nodes match {
          case Nil =>
            words
          case branch :: remainder =>
            if (branch.isTerminated) {
              val children   = branch.children - Leaf.letter
              val newLetters = branch.letter :: letters
              val newWords   = words + newLetters.reverse.mkString

              descend(remainder, letters, descend(children.values.toList, newLetters, newWords))
            } else {
              descend(
                remainder,
                letters,
                descend(branch.children.values.toList, branch.letter :: letters, words)
              )
            }
        }
      }

      descend(
        location.children.values.toList,
        prefix.toCharArray.reverse.toList,
        Set.empty[String]
      )
    }
  }

  trait Node {
    def letter: Char
    def children: Children

    def find(prefix: GraphemeMatcher) = {
      def descend(
        alternates: List[Alternate],
        foundPrefix: List[Char],
        relativeRoot: Node,
        descendants: Children
      ): Set[Path] = {
        alternates match {
          case Nil =>
            Set(Path(foundPrefix.reverse.mkString, relativeRoot))
          case alternate :: remainder =>
            val matchingKeys = descendants.keySet.filter { key =>
              alternate.isDefinedAt(key)
            }

            matchingKeys.flatMap { key =>
              descend(remainder, key :: foundPrefix, descendants(key), descendants(key).children)
            }.toSet
        }
      }

      descend(prefix.indexes.toList, Nil, this, children)
    }

    def find(prefix: String): Option[PrefixSearchTree.Path] = {
      @tailrec
      def descend(letters: List[Char], descendants: Children): Option[Path] = {
        letters match {
          case Nil =>
            None
          case letter :: Nil =>
            descendants.get(letter) map { child => Path(prefix, child) }
          case letter :: remainder =>
            descendants.get(letter) match {
              case Some(descendant) =>
                descend(remainder, descendant.children)
              case _ =>
                None
            }
        }
      }

      descend(prefix.toCharArray.toList, children)
    }

    def contains(prefix: String) = {
      find(prefix).isDefined
    }

    def terminate = {
      children.getOrElseUpdate(Leaf.letter, Leaf)
    }

    def isTerminated = {
      children.contains(Leaf.letter)
    }

    override final def equals(o: Any) = {
      o match {
        case n: Node => n.letter.equals(letter)
        case _ => false
      }
    }

    override final def hashCode = {
      letter.hashCode()
    }
  }

  case class Branch(
    letter: Char,
    children: Children = mutable.Map[Char, Node]()
  ) extends Node

  case object Leaf extends Node {
    val letter = '\0'
    val children = mutable.Map.empty[Char, Node]
  }
}

case class PrefixSearchTree(
  children: Children = mutable.Map[Char, PrefixSearchTree.Node]()
) extends PrefixSearchTree.Node {
  val letter = ' '

  def apply(prefix: String) = {
    find(prefix) map { _.words } getOrElse(Set.empty)
  }

  def apply(prefix: GraphemeMatcher) = {
    find(prefix) flatMap { _.words }
  }

  def add(word: String): Unit = {
    add(word.toCharArray)
  }

  def add(letters: Seq[Char]): Unit = {
    letters.zipWithIndex.foldLeft(this: PrefixSearchTree.Node) { case (node, (letter, index)) =>
      val nodeToAdd = node.children.getOrElseUpdate(letter, Branch(letter))
      if (index + 1 == letters.size) {
        nodeToAdd.terminate
      }
      nodeToAdd
    }
  }
}