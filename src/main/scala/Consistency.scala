abstract class Consistency(val name: String):
  def check(history: History): Boolean

  override def toString: String = name

object Linearizability extends Consistency("Linearizability"):
  override def check(history: History): Boolean = Consistency.totally_ordered(history) && Consistency.real_time_ordered(history)

object SeqCons extends Consistency("SeqCons"):
  override def check(history: History): Boolean = Consistency.process_ordered(history) && Consistency.process_ordered(history)

object Consistency:
  def totally_ordered(history: History): Boolean =
    if history.opExes.isEmpty then return true
    var op = history.opExes.filter(_.prev.isEmpty).head
    var count = 0
    while op.next.nonEmpty do {
      count += 1
      if op.next.size > 1 then return false
      op = op.next.head
    }
    count == history.opExes.size - 1

  def process_ordered(history: History): Boolean =
    val processes = history.opExes.map(_.process)
    processes.forall(p => real_time_ordered(History(history.opExes.filter(_.process == p), history.ordering)))


  def real_time_ordered(history: History): Boolean =
    history.opExes.forall { first =>
      history.opExes.forall { second =>
          first.interval._2 >= second.interval._1 ||
          first.future().contains(second)
      }
    }

  def get_consistency(name: String): Consistency =
    name match
      case "Linearizability" =>  Linearizability
      case "SeqCons" => SeqCons
      case _ => throw new IllegalArgumentException("Consistency type not found: " + name)
