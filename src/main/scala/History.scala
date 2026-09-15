/** An AMECOS history and its partial order over operation executions. */
class History(val opExes: Set[Operation]):

  /** Returns whether every op-ex satisfies its V, S, and L predicates. */
  def legal(order: Order, doPrint: Boolean = false): Boolean = opExes.forall(_.legal(order, doPrint))

