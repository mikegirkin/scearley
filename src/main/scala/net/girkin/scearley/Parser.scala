package net.girkin.scearley

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

case class Rule(left: NonTerminal, expansion: IndexedSeq[RuleString])
case class RuleString(items: IndexedSeq[Symbol]) {
  def append(item: Symbol): RuleString = this.copy(items :+ item)
}

object RuleString {
  def apply(item: Symbol*): RuleString = new RuleString(item.toIndexedSeq)
  def apply(items: IndexedSeq[Symbol]): RuleString = new RuleString(items)
}

case class Grammar(rules: IndexedSeq[Rule]) extends AnyVal

object Grammar {
  def apply(rules: Rule*): Grammar = new Grammar(rules.toVector)
}

object matchers {
  val integer = Terminal(IntMatcher)
  def str(s: String) = Terminal(StringMatcher(s))
}