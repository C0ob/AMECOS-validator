/** An AMECOS history and its partial order over operation executions. */
class History(val opExes: Set[Operation], val ordering: Map[Operation, Set[Operation]]):
  opExes.foreach { o =>
    o.next = ordering.getOrElse(o, Set()) // Add all successors
    ordering.getOrElse(o, Set()).foreach(p => p.prev = p.prev + o) // Invert relation
  }

  /** Returns whether every op-ex satisfies its V, S, and L predicates. */
  def legal(): Boolean = opExes.forall(_.legal())

