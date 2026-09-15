/** Values accepted as operation arguments and results. (only support Int for now) */
type dataType = Int 

/** Runtime types declared by an operation's sequential specification. */
enum DataType:
  /** Integer data. */
  case Int

/** An operation execution (op-ex) in an AMECOS history.
  *
  * Its V/S/L predicates describe validity, safety, and liveness respectively.
  */
abstract class Operation(val process: Process, val obj: Object, val inputTypes: Array[DataType],
                         val outputType: Option[DataType], start: Int, end: Int, val name: String = "") {

  val input: Array[dataType] = Array()
  val output: Option[dataType] = None

  var interval: (Int, Int) = (start, end)
  assert(interval._1 <= interval._2)

  def prev(order: Order): Set[Operation] = order.prev(this)
  def next(order: Order): Set[Operation ] = order.next(this)

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
  def legal(order: Order): Boolean = {
    println("!--- Verifying " + this)
    next(order).foreach(o => println("" + this + "->" + o))
    val resV = v(order)
    println("V is " + resV)
    val resS = s(order)
    println("S is " + resS)
    val resL = l(order)
    println("L is " + resL)
    val res = resV && resS && resL
    if res then println(Console.GREEN + "Legal" + Console.RESET)
    else println(Console.RED + "Illegal" + Console.RESET)
    res
  }

}
