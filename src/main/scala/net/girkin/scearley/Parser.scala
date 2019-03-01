package net.girkin.scearley

import scala.util.Try
import scala.util.matching.Regex



//
//sealed trait Rule {
//  def isTerminal: Boolean
//}
//
//object Rule {
//  case class Disjunction(rules: Seq[Rule]) extends Rule {
//    val isTerminal = false
//  }
//  case class Sequence(rules: Seq[Rule]) extends Rule {
//    val isTerminal = false
//  }
//
//
//  def or(rules: Rule*): Rule = {
//    Disjunction(rules)
//  }
//
//  def seq(rules: Rule*): Rule = {
//    Sequence(rules)
//  }
//
//}
//



trait Symbol {
  def isTerminal: Boolean
}

case class NonTerminal(name: String) extends Symbol {
  val isTerminal = false
}

trait Matcher[T] {
  def tryMatch(input: String): Option[T]
}

case class Terminal[T](matcher: Matcher[T]) extends Symbol {
  val isTerminal = true
}

case object IntMatcher extends Matcher[Int] {
  val re: Regex = re("^[0-9]+$"

  override def tryMatch(input: String): Option[Int] = {
    re.findFirstIn(input).flatMap {
      str =>
        Try {
          str.toInt
        }.toOption
    }
  }
}

case class StringMatcher(str: String) extends Matcher[String] {
  override def tryMatch(input: String): Option[String] = {
    if (input == str) Some(input) else None
  }
}

object test {
  val integer = Terminal(IntMatcher)
  def str(s: String) = Terminal(StringMatcher(s))

  // EXPR ->
  //  int
  //  | "(" EXPR ")"
  //  | EXPR OPERATION EXPR
  //
  // OPERATION ->
  //  "+"
  //  | "-"

  val expr = NonTerminal("EXPR")
  val op = NonTerminal("OP")

  type Grammar = Array[(NonTerminal, Array[Array[Symbol]])]

  val grammar: Grammar = Array(
    expr -> Array[Array[Symbol]](
      Array(integer),
      Array(str("("), expr, str(")")),
      Array(expr, op, expr)
    ),

    op -> Array(
      Array(str("+")),
      Array(str("-"))
    )
  )

}