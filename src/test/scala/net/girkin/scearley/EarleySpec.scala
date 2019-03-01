package net.girkin.scearley

import org.scalatest.{FlatSpec, FunSpec, Matchers, WordSpec}

class EarleySpec extends FunSpec with Matchers{

  it("parses the easiest thing") {
    val content = "1"

    earley.parse(test.expr, content) shouldBe Boolean
  }

}
