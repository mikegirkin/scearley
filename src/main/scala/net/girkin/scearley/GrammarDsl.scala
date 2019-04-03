package net.girkin.scearley

object GrammarDsl {
  def NT(name: String) = NonTerminal(name)

  implicit class NonTerminalOps(val s: NonTerminal) extends AnyVal {
    def :-(that: Symbol): Rule = {
      Rule(s, IndexedSeq(RuleString(that)))
    }

    def :-(that: RuleString): Rule = {
      Rule(s, IndexedSeq(that))
    }

    def :-(that: IndexedSeq[RuleString]): Rule = {
      Rule(s, that)
    }
  }

  implicit class SymbolOps(val s: Symbol) extends AnyVal {
    def ~(that: Symbol): RuleString = {
      RuleString(s, that)
    }
  }

  implicit class RuleStringOps(val rs: RuleString) extends AnyVal {
    def ~(that: Symbol): RuleString = {
      rs.append(that)
    }

    def or(that: RuleString): IndexedSeq[RuleString] = {
      IndexedSeq(rs, that)
    }
  }

  implicit class RulesOps(val items: IndexedSeq[RuleString]) extends AnyVal {
    def or(that: RuleString): IndexedSeq[RuleString] = {
      items :+ that
    }
  }
}
