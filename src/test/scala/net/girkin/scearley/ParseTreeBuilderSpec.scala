package net.girkin.scearley

import net.girkin.scearley.matchers.{integer, str}
import org.scalatest.{FunSpec, Matchers}

class ParseTreeBuilderSpec extends FunSpec with Matchers {
  import GrammarDsl._

  describe("Building valid trees for simple EXPR grammars") {

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
      "1 + 1".split(' ').toList ->
        ParseTreeNonTerminal(NT("MAIN"), 0, 3, Seq(
          ParseTreeNonTerminal(NT("EXPR"), 0, 3, Seq(
            ParseTreeNonTerminal(NT("EXPR"), 0, 1, Seq(
              ParseTreeTerminal(integer, 0)
            )),
            ParseTreeNonTerminal(NT("OP"), 1, 2, Seq(
              ParseTreeTerminal(str("+"), 1)
            )),
            ParseTreeNonTerminal(NT("EXPR"), 2, 3, Seq(
              ParseTreeTerminal(integer, 2)
            ))
          ))
        )),
      "1 + 2 + 3".split(' ').toList ->
        ParseTreeNonTerminal(NT("MAIN"), 0, 5, Seq(
          ParseTreeNonTerminal(NT("EXPR"), 0, 5, Seq(
            ParseTreeNonTerminal(NT("EXPR"), 0, 3, Seq(
              ParseTreeNonTerminal(NT("EXPR"), 0, 1, Seq(ParseTreeTerminal(integer, 0))),
              ParseTreeNonTerminal(NT("OP"), 1, 2, Seq(ParseTreeTerminal(str("+"), 1))),
              ParseTreeNonTerminal(NT("EXPR"), 2, 3, Seq(ParseTreeTerminal(integer, 2))),
            )),
            ParseTreeNonTerminal(NT("OP"), 3, 4, Seq(
              ParseTreeTerminal(str("+"), 3 )
            )),
            ParseTreeNonTerminal(NT("EXPR"), 4, 5, Seq(
              ParseTreeTerminal(integer, 4)
            ))
          ))
        )),
      "( 1 + 2 ) - 3".split(' ').toList ->
        ParseTreeNonTerminal(NT("MAIN"), 0, 7, Seq(
          ParseTreeNonTerminal(NT("EXPR"), 0, 7, Seq(
            ParseTreeNonTerminal(NT("EXPR"), 0, 5, Seq(
              ParseTreeTerminal(str("("), 0),
              ParseTreeNonTerminal(NT("EXPR"), 1, 4, Seq(
                ParseTreeNonTerminal(NT("EXPR"), 1, 2, Seq(ParseTreeTerminal(integer, 1))),
                ParseTreeNonTerminal(NT("OP"), 2, 3, Seq(ParseTreeTerminal(str("+"), 2))),
                ParseTreeNonTerminal(NT("EXPR"), 3, 4, Seq(ParseTreeTerminal(integer, 3))),
              )),
              ParseTreeTerminal(str(")"), 4)
            )),
            ParseTreeNonTerminal(NT("OP"), 5, 6, Seq(
              ParseTreeTerminal(str("-"), 5)
            )),
            ParseTreeNonTerminal(NT("EXPR"), 6, 7, Seq(
              ParseTreeTerminal(integer, 6)
            ))
          ))
        )),
    )

    val parser = new Earley()

    for {
      (input, expected) <- tests
    } {
      val table = parser.parse(grammar, input).table
      val completedTable = table.mapValues { v =>
        v.filter( er =>
          er.finished
        )
      }
      val result = ParseTreeBuilder.build(table)

      it(s"should work fine with $input") {
        result.size shouldBe 1
        result.head shouldBe expected
      }
    }

  }

}
