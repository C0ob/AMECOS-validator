/** Values accepted as operation arguments and results. (only support Int for now) */
/** Runtime representation of values accepted by operation arguments/results. */
type dataType = Int | String

/** An operation execution (op-ex) in an AMECOS history.
 *
 * Its V/S/L predicates describe validity, safety, and liveness respectively.
 */
abstract class Operation(val process: Process, val obj: Object, val inputTypes: Array[DataType],
                         val outputType: Option[DataType], start: Int, end: Int, val name: String = "") {

  /** Concrete argument values supplied to this operation. */
  val input: Array[dataType] = Array()
  /** Concrete result returned by this operation, when any. */
  val output: Option[dataType] = None

  /** Invocation and response timestamps, inclusive. */
  var interval: (Int, Int) = (start, end)
  assert(interval._1 <= interval._2)

  /** Direct predecessors of this operation in an order. */
  def prev(order: Order): Set[Operation] = order.prev(this)

  /** Direct successors of this operation in an order. */
  def next(order: Order): Set[Operation] = order.next(this)

  /** Operations ordered before this op-ex by the transitive partial order. */
  def context(order: Order): Set[Operation] = order.context(this)

  /** Operations ordered after this op-ex by the transitive partial order. */
  def future(order: Order): Set[Operation] = order.future(this)

  /** AMECOS validity predicate. */
  def v(order: Order): Boolean

  /** AMECOS safety predicate. */
  def s(order: Order): Boolean

  /** AMECOS liveness predicate. */
  def l(order: Order): Boolean

  /** Formats the operation, result, and execution interval for diagnostics. */
  override def toString: String = name + input.mkString("(", ", ", ")") + "/" + output.mkString + " " + interval.toString()

  /** Checks the op-ex's V, S, and L predicates. */
  def legal(order: Order, doPrint: Boolean = false): Boolean = {
    if doPrint then println("!--- Verifying " + this)
    val resV = v(order)
    if doPrint then println("V is " + resV)
    val resS = s(order)
    if doPrint then println("S is " + resS)
    val resL = l(order)
    if doPrint then println("L is " + resL)
    val res = resV && resS && resL
    if doPrint then {
      if res then println(Console.GREEN + "Legal" + Console.RESET)
      else println(Console.RED + "Illegal" + Console.RESET)
    }
    res
  }

}

/** Runtime types declared by an operation's sequential specification. */
enum DataType:
  /** Integer data. */
  case Int, String
