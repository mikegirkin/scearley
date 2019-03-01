package net.girkin.scearley

import net.girkin.scearley.test.Grammar

import scala.collection._

object Earley {
  case class EarleyRecord(rule: (NonTerminal, Array[Symbol]) , dotPosition: Int, startPosition: Int)

  def parse(grammar: Grammar, content: String): Boolean = {
    val start = EarleyRecord((grammar), 0, 0)

    val table = mutable.ArrayBuffer[EarleyRecord]()

    table.append(start)

    for (char <- content) {
      processChar(char)
    }

    def processChar(c: Char)= {
      for (ruleState <- table) {
        if(!finihed(ruleState)) {
          if(isNextElementTerminal(ruleState)) {
            scanner()
          } else {
            predictor()
          }
        } else {
          completer()
        }
      }
    }

    def isNextElementTerminal(ruleState: EarleyRecord): Boolean = {
      ruleState.rule match {
        case Rule.Sequence(rules) =>
          rules(ruleState.dotPosition).isTerminal
      }
    }

    def finihed(record: EarleyRecord): Boolean = ???

    def scanner() = ???

    def predictor() = ???

    def completer() = ???
  }
}
