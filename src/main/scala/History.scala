class History(val opExes: Set[Operation], val ordering: Map[Operation, Set[Operation]]):
  opExes.foreach { o =>
    o.next = ordering.getOrElse(o, Set()) // Add all successors
    ordering.getOrElse(o, Set()).foreach(p => p.prev = p.prev + o) // Invert relation
  }

  def legal(): Boolean = opExes.forall(_.legal())


