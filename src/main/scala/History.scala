class History(val opExes: Set[Operation], val ordering: Map[Operation, Set[Operation]]):
  opExes.foreach{ o => o.next = ordering.getOrElse(o, Set())
    ordering.getOrElse(o, Set()).foreach(p => p.prev = p.prev + o)
  }
  def legal(): Boolean = opExes.forall(_.legal())


