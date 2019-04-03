package net.girkin.scearley

import scala.collection._
import scala.collection.mutable.ArrayBuffer

case class EarleyRecord(
  ruleLeft: NonTerminal,
  ruleExpansion: RuleString,
  dotPosition: Int,
  startPosition: Int
) {
  def currentSymbol: Option[Symbol] = ruleExpansion.items.lift(dotPosition)

  def finished: Boolean = {
    dotPosition == ruleExpansion.items.length
  }
}

case class ParsingResult(
  isCorrect: Boolean,
  table: Map[Int, IndexedSeq[EarleyRecord]]
)

class Earley {
  type TInToken = String

  private def scanner(ruleState: EarleyRecord, symbol: Terminal, currentSymbolIndex: Int, word: Option[TInToken]): Option[(Int, EarleyRecord)] = {
    word.flatMap { w =>
      symbol.matcher.tryMatch(w).map { _ =>
        (
          currentSymbolIndex + 1,
          ruleState.copy(
            dotPosition = ruleState.dotPosition + 1
          )
        )
      }
    }
  }

  private def predictor(table: Map[Int, IndexedSeq[EarleyRecord]], grammar: Grammar, ruleState: EarleyRecord, currentSymbolIndex: Int): Seq[(Int, EarleyRecord)] = {
    val expandingSymbol = ruleState.currentSymbol
    val applicableRules: IndexedSeq[(NonTerminal, RuleString)] =
      grammar.rules
        .filter(r => expandingSymbol.contains(r.left))
        .flatMap(r => r.expansion.map(ruleLine => r.left -> ruleLine))

    applicableRules.map { case (symbolLeft, ruleExpansion) =>
      currentSymbolIndex -> EarleyRecord(symbolLeft, ruleExpansion, 0, currentSymbolIndex)
    }
  }

  private def completer(table: Map[Int, IndexedSeq[EarleyRecord]], completingRuleState: EarleyRecord, currentSymbolIndex: Int): Seq[(Int, EarleyRecord)] = {
    for {
      ruleState <- table(completingRuleState.startPosition).filter(rule => rule.currentSymbol.contains(completingRuleState.ruleLeft))
    } yield {
      currentSymbolIndex -> ruleState.copy(
        dotPosition = ruleState.dotPosition + 1
      )
    }
  }


  def addToTable(table: mutable.Map[Int, mutable.ArrayBuffer[EarleyRecord]], items: Seq[(Int, EarleyRecord)]): Unit = {
    for {
      (index, ruleState) <- items
    } {
      if (!table.contains(index)) {
        table(index) = new mutable.ArrayBuffer[EarleyRecord]
        table(index).append(ruleState)
      } else if (!table(index).contains(ruleState)) {
        table(index).append(ruleState)
      }
    }
  }

  def parse(grammar: Grammar, content: Seq[TInToken]): ParsingResult = {

    val table: mutable.Map[Int, mutable.ArrayBuffer[EarleyRecord]] = mutable.Map[Int, mutable.ArrayBuffer[EarleyRecord]]()

    val start = EarleyRecord(grammar.rules(0).left, grammar.rules(0).expansion(0), 0, 0)
    table(0) = new mutable.ArrayBuffer()
    table(0).append(start)

    for (index <- Range(0, content.length+1)) {
      val word = content.lift(index)

      var currentPosition = 0
      while (currentPosition < table.getOrElse(index, ArrayBuffer.empty).length) {
        val ruleState = table.getOrElse(index, ArrayBuffer.empty)(currentPosition)
        if (!ruleState.finished) {
          ruleState.currentSymbol match {
            case Some(symbol @ Terminal(_)) => {
              val newRules = scanner(ruleState, symbol, index, word)
              addToTable(table, newRules.toSeq)
            }
            case Some(NonTerminal(_)) => {
              val newRules = predictor(table, grammar, ruleState, index)
              addToTable(table, newRules)
            }
          }
        } else {
          val newRules = completer(table, ruleState, index)
          addToTable(table, newRules)
        }
        currentPosition += 1
      }
    }

    val isCorrect = table.get(content.size).fold(
      false
    )(
      _.exists(item =>
        item.ruleLeft == start.ruleLeft && item.dotPosition == item.ruleExpansion.items.size && item.startPosition == 0
      )
    )

    ParsingResult(isCorrect, table)
  }
}
