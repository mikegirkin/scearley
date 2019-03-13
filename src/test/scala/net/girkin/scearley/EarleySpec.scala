package net.girkin.scearley

import net.girkin.scearley.matchers._
import net.girkin.scearley.test.Grammar
import org.scalatest.{FunSpec, Matchers}

class EarleySpec extends FunSpec with Matchers{

  val earley = new Earley()

  describe("parsing 1 rule 1 terminal grammar") {
    val tests = Seq(
      Seq("1") -> true,
      Seq("1", "1") -> false,
      Seq("ab") -> false
    )

    val grammar: Grammar = Vector[Rule](
      Rule(NonTerminal("MAIN"), Vector(Vector(integer)))
    )

    for {
      (input, expected) <- tests
    } {
      it(s"parses the easiest thing ${input}") {
        earley.parse(grammar, input) shouldBe expected
      }
    }
  }

  describe("parsing with grammar 1 rule of 3 non-terminals") {
    val tests = Seq(
      Seq("1" , "+", "1") -> true,
      Seq("11", "12") -> false,
      Seq("abc", "abc", "abc") -> false
    )

    val grammar: Grammar = Vector[Rule](
      Rule(NonTerminal("MAIN"), Vector(
        Vector(integer, str("+"), integer)))
    )

    for {
      (input, expected) <- tests
    } {
      it(s"parses correctly ${input}") {
        earley.parse(grammar, input) shouldBe expected
      }
    }
  }

}
