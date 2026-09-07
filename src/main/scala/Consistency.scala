/** A consistency condition evaluated over an AMECOS history. */
abstract class Consistency(val name: String):
  /** Tests whether this condition holds for the supplied history. */
  def check(history: History): Boolean

  /** Returns the model's display name. */
  override def toString: String = name

/** AMECOS condition requiring a total order that respects real-time ordering. */
object Linearizability extends Consistency("Linearizability"):
  /** Checks total ordering and real-time ordering of the history. */
  override def check(history: History): Boolean = Consistency.totally_ordered(history) && Consistency.real_time_ordered(history)

/** AMECOS condition requiring each process's operations to respect real-time order. */
object SeqCons extends Consistency("SeqCons"):
  /** Checks real-time ordering independently for each process. */
  override def check(history: History): Boolean = Consistency.process_ordered(history) && Consistency.process_ordered(history)

/** Implementations of the ordering predicates used by consistency models. */
object Consistency:
  /** Checks whether the supplied partial order is a single total chain. */
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

  /** Checks real-time ordering for each process projection of the history. */
  def process_ordered(history: History): Boolean =
    val processes = history.opExes.map(_.process)
    processes.forall(p => real_time_ordered(History(history.opExes.filter(_.process == p), history.ordering)))


  /** Checks that non-overlapping op-exes occur in real-time order. */
  def real_time_ordered(history: History): Boolean =
    history.opExes.forall { first =>
      history.opExes.forall { second =>
          first.interval._2 >= second.interval._1 ||
          first.future().contains(second)
      }
    }

  /** Resolves a consistency name from the `.amecos` DSL. */
  def get_consistency(name: String): Consistency =
    name match
      case "Linearizability" =>  Linearizability
      case "SeqCons" => SeqCons
      case _ => throw new IllegalArgumentException("Consistency type not found: " + name)
