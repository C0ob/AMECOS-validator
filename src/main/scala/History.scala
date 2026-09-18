/** An AMECOS history and its partial order over operation executions. */
class History(val opExes: Set[Operation]):

  /** Tests all operations against the supplied partial order. */
  def legal(order: Order, doPrint: Boolean = false): Boolean = opExes.forall(_.legal(order, doPrint))
