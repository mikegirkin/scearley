package net.girkin.scearley

import net.girkin.scearley.matchers._
import org.scalatest.{FunSpec, Matchers}

class EarleySpec extends FunSpec with Matchers{

  val earley = new Earley()

  describe("parsing 1 rule 1 terminal grammar") {
    val tests = Seq(
      Seq("1") -> true,
      Seq("1", "1") -> false,
      Seq("ab") -> false
    )

    val grammar: Grammar = Grammar(
      Rule(NonTerminal("MAIN"), Vector(RuleString(integer)))
    )

    for {
      (input, expected) <- tests
    } {
      it(s"parses the easiest thing ${input}") {
        earley.parse(grammar, input).isCorrect shouldBe expected
      }
    }
  }

  describe("parsing with grammar 1 rule of 3 terminals") {
    val tests = Seq(
      Seq("1" , "+", "1") -> true,
      Seq("11", "12") -> false,
      Seq("abc", "abc", "abc") -> false
    )

    val grammar: Grammar = Grammar(
      Rule(NonTerminal("MAIN"), Vector(
        RuleString(integer, str("+"), integer)))
    )

    for {
      (input, expected) <- tests
    } {
      it(s"parses correctly ${input}") {
        earley.parse(grammar, input).isCorrect shouldBe expected
      }
    }
  }

  describe("parsing using grammar [ EXPR :- int OP int ]") {
    // EXPR :- int OP int
    // OP :- '+'
   //      | '-'
    val grammar: Grammar = Grammar(
      Rule(NonTerminal("EXPR"), Vector(
        RuleString(integer, NonTerminal("OP"), integer)
      )),
      Rule(NonTerminal("OP"), Vector(
        RuleString(str("-")),
        RuleString(str("+"))
      ))
    )

    val tests = Seq(
      Seq("1", "+", "1") -> true,
      Seq("155", "-", "12") -> true,
      Seq("-", "12314", "+", "1234") -> false,
      Seq("155", "-", "12", "123") -> false,
      Seq("155", "asdj", "12", "123") -> false,
    )

    for {
      (input, expected) <- tests
    } {
      it(s"parses correctly ${input}") {
        earley.parse(grammar, input).isCorrect shouldBe expected
      }
    }
  }

  describe("parsing using EXPR recursive grammar") {
    // EXPR :- EXPR OP EXPR
    //       | 'int
    //       | ( EXPR )
    // OP :- +
    //     | -
    //
    import GrammarDsl._

    val grammar = Grammar(
      NT("MAIN") :- NT("EXPR"),
      NT("EXPR") :- {
        RuleString(integer) or
        (str("(") ~ NT("EXPR") ~ str(")")) or
        (NT("EXPR") ~ NT("OP") ~ NT("EXPR"))
      },
      NT("OP") :- {
        RuleString(str("+")) or
        RuleString(str("-"))
      }
    )

    val tests = Seq(
      Seq("1") -> true,
      Seq("1", "+", "1") -> true,
      Seq("(", "1", ")") -> true,
      Seq("(", "1", "+", "2", ")") -> true,
      Seq("(", "1", "+", "2", ")", "-", "12") -> true,
      Seq("(", "1", "+", "(", "23", "+", "2", ")", ")", "-", "12") -> true,
      Seq("(", "1", "+", "(", "23", "+", "2", ")", "-", "12") -> false,
      Seq("(", "1", "+", "(", "23", "2", ")", ")", "-", "12") -> false,
      Seq("(", "1", "+", "(", "23", "+", "2", ")", ")", "-", "abc") -> false,
    )

    for {
      (input, expected) <- tests
    } {
      it(s"parses correctly ${input}") {
        val result = earley.parse(grammar, input)
        result.isCorrect shouldBe expected

        val tree = ParseTreeBuilder.build(result.table)
      }
    }
  }
}
