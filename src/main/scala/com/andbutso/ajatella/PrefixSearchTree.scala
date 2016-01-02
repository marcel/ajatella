package com.andbutso.ajatella

import com.andbutso.ajatella.PrefixSearchTree.{Branch, Children}

import scala.annotation.tailrec
import scala.collection.mutable

object PrefixSearchTree {
  type Children = mutable.Map[Char, Node]
  val EmptyChildren = mutable.Map[Char, Node]()
  val TerminatedChild = EmptyChildren += Leaf.letter -> Leaf

  case class Path(prefix: String, location: Node) {
    def words = {
      def descend(
        nodes: List[Node],
        letters: List[Char],
        words: Set[String]
      ): Set[String] = {
//        println("-"*100)
        nodes match {
          case Nil =>
//            println("+ Nil")
//            println(s"  nodes: $nodes")
//            println(s"  words: $words")
//            println(s"  letters: $letters")
//            println("-> words")
            words
          case Leaf :: remainder =>
//            println("+ Leaf :: remainder")
//            println(s"  nodes: $nodes")
//            println(s"  words: $words")
//            println(s"  letters: $letters")
//            println(s"  remainder: $remainder")
//            println("-> descend(remainder, prefix, words)")
            descend(remainder, letters, words + letters.reverse.mkString)
          case Branch(letter, TerminatedChild) :: remainder =>
//            println("+ Branch(letter, TerminatedChild) :: remainder")
//            println(s"  letter: $letter")
//            println(s"  nodes: $nodes")
//            println(s"  words: $words")
//            println(s"  letters: $letters")
//            println(s"  remainder: $remainder")
            val newLetters = letter :: letters
//            println(s"  newLetters: $newLetters")
//            println("-> descend(remainder, newPrefix, words + newPrefix.reverse.mkString)")
            descend(remainder, newLetters, words + newLetters.reverse.mkString)
          case Branch(letter, descendants) :: remainder =>
//            println("+ Branch(letter, descendants) :: remainder")
//            println(s"  letter: $letter")
//            println(s"  nodes: $nodes")
//            println(s"  words: $words")
//            println(s"  letters: $letters")
//            println(s"  remainder: $remainder")
//            println(s"  descendants: $descendants")
//            println("-> descend(descendants.values.toList, letter :: letters, words)")
            descend(remainder, letters, descend(descendants.values.toList, letter :: letters, words))
          case TerminatedChild :: Nil =>
            descend(Nil, letters, words + letters.reverse.mkString)
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


    def find(letters: Letters) = ??? // TODO Implement support for return paths that match Alternates

    def find(prefix: String): Option[PrefixSearchTree.Path] = {
      @tailrec
      def descend(letters: List[Char], descendants: Children): Option[Path] = {
        letters match {
          case Nil =>
            None
          case letter :: Nil =>
            descendants.get(letter) map { child => Path(prefix, child) }
          case letter :: remainder =>
            val descendant = descendants.get(letter)
            descend(remainder, descendant.map { _.children } getOrElse EmptyChildren)
        }
      }

      descend(prefix.toCharArray.toList, children)
    }

    def terminate = {
      children.getOrElseUpdate(Leaf.letter, Leaf)
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