package net.girkin.scearley

import net.girkin.scearley.test.Grammar

import scala.collection._

class Earley {
  type TInToken = String

  case class EarleyRecord(ruleLeft: NonTerminal, ruleExpansion: Array[Symbol], dotPosition: Int, startPosition: Int)

  private def processChar(index: Int, word: TInToken)= {
    for (ruleState <- table(index)) {
      if(!finihed(ruleState)) {
        nextElementOf(ruleState) match {
          case s @ Terminal(_) => scanner(ruleState, s, index, word)
          case s @ NonTerminal(_) => ???
        }
      } else {
        completer()
      }
    }
  }

  private def nextElementOf(ruleState: EarleyRecord): Symbol = {
    ruleState.ruleExpansion(ruleState.dotPosition)
  }

  private def isNextElementTerminal(ruleState: EarleyRecord): Boolean = {
    nextElementOf(ruleState).isTerminal
  }

  private def finihed(record: EarleyRecord): Boolean = ???

  private def scanner(ruleState: EarleyRecord, symbol: Terminal, index:Int, word: TInToken): Option[(Int, EarleyRecord)] = {
    symbol.matcher.tryMatch(word).map { _ =>
      (
        index + 1,
        ruleState.copy(
          dotPosition = ruleState.dotPosition + 1,
        )
      )
    }
  }

  private def predictor() = ???

  private def completer() = ???


  def parse(grammar: Grammar, content: Seq[TInToken]): Boolean = {

    val table = mutable.ArrayBuffer[mutable.ArrayBuffer[EarleyRecord]]()

    val start = EarleyRecord(grammar(0).left, grammar(0).expansion(0), 0, 0)
    table.append(new mutable.ArrayBuffer())
    table(0).append(start)

    for (index <- content.indices) {
      val word = content(index)
      processChar(index, word)
    }
  }
}
