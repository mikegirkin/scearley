package net.girkin.scearley

import net.girkin.scearley.test.Grammar

import scala.collection._
import scala.collection.mutable.ArrayBuffer

case class EarleyRecord(ruleLeft: NonTerminal, ruleExpansion: IndexedSeq[Symbol], dotPosition: Int, startPosition: Int)

class Earley {
  type TInToken = String

  private def nextElementOf(ruleState: EarleyRecord): Symbol = {
    ruleState.ruleExpansion(ruleState.dotPosition)
  }

  private def isNextElementTerminal(ruleState: EarleyRecord): Boolean = {
    nextElementOf(ruleState).isTerminal
  }

  private def finihed(ruleState: EarleyRecord): Boolean = {
    ruleState.dotPosition == ruleState.ruleExpansion.length
  }

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

  private def completer(table: Map[Int, IndexedSeq[EarleyRecord]], completingRuleState: EarleyRecord, currentIndex: Int):Seq[(Int, EarleyRecord)] = {
    for {
      ruleState <- table(completingRuleState.startPosition).filter(rule => rule.ruleExpansion(rule.dotPosition) == completingRuleState.ruleLeft)
    } yield {
      currentIndex -> ruleState.copy(
        dotPosition = ruleState.dotPosition + 1
      )
    }
  }


  def parse(grammar: Grammar, content: Seq[TInToken]): Boolean = {

    val table: mutable.Map[Int, mutable.ArrayBuffer[EarleyRecord]] = mutable.Map[Int, mutable.ArrayBuffer[EarleyRecord]]()

    def addToTable(items: (Int, EarleyRecord)*): Unit = {
      for {
        (index, ruleState) <- items
      } {
        if (table.contains(index)) table(index).append(ruleState)
        else {
          table(index) = new mutable.ArrayBuffer[EarleyRecord]
          table(index).append(ruleState)
        }
      }
    }

    def processChar(index: Int, word: TInToken): Unit = {
      for (ruleState <- table.getOrElse(index, ArrayBuffer.empty)) {
        if(!finihed(ruleState)) {
          nextElementOf(ruleState) match {
            case symbol @ Terminal(_) => {
              val newRules = scanner(ruleState, symbol, index, word)
              addToTable(newRules.toSeq:_*)
            }
            case s @ NonTerminal(_) => ???
          }
        } else {
          completer(table, ruleState, index)
        }
      }
    }

    val start = EarleyRecord(grammar(0).left, grammar(0).expansion(0), 0, 0)
    table(0) = new mutable.ArrayBuffer()
    table(0).append(start)

    for (index <- content.indices) {
      val word = content(index)
      processChar(index, word)
    }

    println(table.mkString("Map (", System.lineSeparator(), ")"))

    table.get(content.size).fold(
      false
    )( _.exists( item =>
      item.ruleLeft == start.ruleLeft && item.dotPosition == item.ruleExpansion.size && item.startPosition == 0
    ))
  }
}
