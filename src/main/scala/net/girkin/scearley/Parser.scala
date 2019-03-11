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
}

case object IntMatcher extends Matcher[String, String] {
  val re: Regex = re("^[0-9]+$")

  override def tryMatch(input: String): Option[String] = {
    re.findFirstIn(input)
  }
}

case class StringMatcher(str: String) extends Matcher[String, String] {
  override def tryMatch(input: String): Option[String] = {
    if (input == str) Some(input) else None
  }
}

case class Rule(left: NonTerminal, expansion: Array[Array[Symbol]])

object matchers {
  val integer = Terminal(IntMatcher)
  def str(s: String) = Terminal(StringMatcher(s))
}

object test {
  import matchers._

  val expr = NonTerminal("EXPR")
  val op = NonTerminal("OP")

  type Grammar = Array[Rule]

  val grammar: Grammar = Array[Rule](
    Rule(NonTerminal("MAIN"), Array(Array(expr))),
    Rule(expr, Array[Array[Symbol]](
      Array(integer),
      Array(str("("), expr, str(")")),
      Array(expr, op, expr)
    )),

    Rule(op, Array(
      Array(str("+")),
      Array(str("-"))
    ))
  )

}