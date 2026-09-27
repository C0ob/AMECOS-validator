/** A consistency condition evaluated over an AMECOS history. */
sealed abstract class Consistency(val name: String):
  /** Consistency level, from 0 (weakest) to 2 (strongest). If level 1 is invalid for an order, all levels above are also invalid. */
  val level: Int

  /** Tests whether this condition holds for the supplied history. */
  def check(order: Order): Boolean

  /** Returns the model's display name. */
  override def toString: String = name

/** AMECOS condition requiring a total order that respects real-time ordering. */
object Atomic extends Consistency("Linearizability"):
  val level = 2

  override def check(order: Order): Boolean = Consistency.totally_ordered(order) && Consistency.real_time_ordered(order, order.history.opExes)


/** AMECOS condition requiring each process's operations to respect real-time order while enforcing a total order. */
object Sequential extends Consistency("SeqCons"):
  val level = 1

  override def check(order: Order): Boolean = Consistency.totally_ordered(order) && Consistency.process_ordered(order)

/** AMECOS condition requiring each process's operations to respect real-time order but does not enforce a total order. */
object Causal extends Consistency("CausalCons"):
  val level = 0

  override def check(order: Order): Boolean = Consistency.process_ordered(order)

/** Implementations of the ordering predicates used by consistency models. */
object Consistency:
  /** Checks whether the supplied partial order is a single total chain. */

  /** Returns whether every operation belongs to one total chain. */
  def totally_ordered(order: Order): Boolean =
    val opExes = order.history.opExes
    if opExes.isEmpty then return true
    var op = opExes.filter(_.prev(order).isEmpty).head
    var count = 0
    while op.next(order).nonEmpty do {
      count += 1
      if op.next(order).size > 1 then return false
      op = op.next(order).head
    }
    count == opExes.size - 1

  /** Checks real-time ordering for each process projection of the history. */

  /** Returns whether each process projection respects real-time order. */
  def process_ordered(order: Order): Boolean =
    val history = order.history
    val processes = history.opExes.map(_.process)
    processes.forall(p => real_time_ordered(order, history.opExes.filter(_.process == p)))


  /** Checks that non-overlapping op-exes occur in real-time order. */

  /** Returns whether non-overlapping operations occur in real-time order. */
  def real_time_ordered(order: Order, processOps: Set[Operation]): Boolean =
    processOps.forall { first =>
      processOps.forall { second =>
        first.interval._2 >= second.interval._1 ||
          first.future(order).contains(second)
      }
    }

  /** Resolves a consistency name from the `.amecos` DSL. */
  def get_consistency(name: String): Consistency =
    name match
      case "Atomic" => Atomic
      case "Sequential" => Sequential
      case "Causal" => Causal
      case _ => throw new IllegalArgumentException("Consistency type not found: " + name)
