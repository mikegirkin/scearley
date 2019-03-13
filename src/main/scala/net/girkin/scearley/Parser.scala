package net.girkin.scearley

import scala.util.Try
import scala.util.matching.Regex

trait Symbol {
  def isTerminal: Boolean
}

case class NonTerminal(name: String) extends Symbol {
  val isTerminal = false
}

trait Matcher[TIn, TOut] {
  def tryMatch(input: TIn): Option[TOut]
}

case class Terminal(matcher: Matcher[String, String]) extends Symbol {
  val isTerminal = true

  //override def toString: String = s"Terminal(${matcher.toString})"
}

case object IntMatcher extends Matcher[String, String] {
  val regex: Regex = "^[0-9]+$".r

  override def tryMatch(input: String): Option[String] = {
    regex.findFirstIn(input)
  }
}

case class StringMatcher(str: String) extends Matcher[String, String] {
  override def tryMatch(input: String): Option[String] = {
    if (input == str) Some(input) else None
  }
}

case class Rule(left: NonTerminal, expansion: IndexedSeq[IndexedSeq[Symbol]])

object matchers {
  val integer = Terminal(IntMatcher)
  def str(s: String) = Terminal(StringMatcher(s))
}

object test {
  import matchers._

  val expr = NonTerminal("EXPR")
  val op = NonTerminal("OP")

  type Grammar = IndexedSeq[Rule]

  val grammar: Grammar = Vector[Rule](
    Rule(NonTerminal("MAIN"), Vector(Vector(expr))),
    Rule(expr, Vector[Vector[Symbol]](
      Vector(integer),
      Vector(str("("), expr, str(")")),
      Vector(expr, op, expr)
    )),

    Rule(op, Vector(
      Vector(str("+")),
      Vector(str("-"))
    ))
  )

}