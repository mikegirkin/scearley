package net.girkin.scearley

import net.girkin.scearley.matchers._
import net.girkin.scearley.test.Grammar
import org.scalatest.{FunSpec, Matchers}

class EarleySpec extends FunSpec with Matchers{

  it("parses the easiest thing") {
    val content = Seq("1")
    val earley = new Earley()

    val grammar: Grammar = Array[Rule](
      Rule(NonTerminal("MAIN"), Array(Array(integer)))
    )

    earley.parse(grammar, content) shouldBe Boolean
  }

}
