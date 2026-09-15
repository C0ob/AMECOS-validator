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

  var prev: Set[Operation] = Set() // Derived from partial order
  var next: Set[Operation] = Set()

  /** Operations ordered before this op-ex by the transitive partial order. */
  def context(slave: Boolean = false): Set[Operation] =
    if prev.isEmpty then if slave then Set(this) else Set()
    else prev ++ prev.flatMap(o => o.context(true))

  /** Operations ordered after this op-ex by the transitive partial order. */
  def future(slave: Boolean = false): Set[Operation] =
    if next.isEmpty then if slave then Set(this) else Set()
    else next ++ next.flatMap(o => o.future(true))

  /** AMECOS validity predicate. */
  def v(): Boolean

  /** AMECOS safety predicate. */
  def s(): Boolean

  /** AMECOS liveness predicate. */
  def l(): Boolean

  /** Formats the operation, result, and execution interval for diagnostics. */
  override def toString: String = name + input.mkString("(", ", ", ")") + "/" + output.mkString + " " + interval.toString()

  /** Checks the op-ex's V, S, and L predicates. */
  def legal(): Boolean = {
    println("!--- Verifying " + this)
    next.foreach(o => println("" + this + "->" + o))
    val resV = v()
    println("V is " + resV)
    val resS = s()
    println("S is " + resS)
    val resL = l()
    println("L is " + resL)
    val res = resV && resS && resL
    if res then println(Console.GREEN + "Legal" + Console.RESET)
    else println(Console.RED + "Illegal" + Console.RESET)
    res
  }

}
