package net.girkin.scearley

trait ParseTreeNode {
  def symbol: Symbol
}

case class ParseTreeNonTerminal(
  symbol: NonTerminal,
  begin: Int,
  end: Int,
  expansion: Seq[ParseTreeNode]
) extends ParseTreeNode

case class ParseTreeTerminal(
  symbol: Terminal,
  tokenIndex: Int
) extends ParseTreeNode

object ParseTreeBuilder {
  def build(chart: Map[Int, IndexedSeq[EarleyRecord]]): Seq[ParseTreeNode] = {

    val completed = chart.mapValues {
      v => v.filter(_.finished)
    }

    val rootNonTerminal = chart(0)(0).ruleLeft
    val lastIndex = completed.keys.max
    completed(lastIndex)
      .filter(r => r.ruleLeft == rootNonTerminal)
      .map { //TODO: Fix that
        er => buildTree(er, completed)
      }
  }

  def buildTree(rootRecord: EarleyRecord, chart: Map[Int, IndexedSeq[EarleyRecord]]): ParseTreeNode = {

    val possibleChildren = rootRecord.parsingMetadata.flatMap {
      item => item.symbol match {
        case s@Terminal(_) =>
          Some(ParseTreeTerminal(
            s, item.begin
          ))
        case s@NonTerminal(_) =>
          chart(item.end).find { er =>
            er.ruleLeft == s &&
              er.startPosition == item.begin &&
              er.finished
          }.map {
            er => buildTree(er, chart)
          }
      }
    }

    ParseTreeNonTerminal(
      rootRecord.ruleLeft,
      rootRecord.startPosition,
      rootRecord.parsingMetadata.last.end,
      possibleChildren
    )
  }
}


