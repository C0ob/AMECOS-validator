import java.util.UUID

type dataType = Int | Boolean | String

enum DataType:
  case Int, Boolean, String

// Represents an operation. Objects of this class are op-exes.
abstract class Operation(val inputTypes: Array[DataType],
                         val outputType: Option[DataType], val name: String = "") {

  val input: Array[dataType] = Array()
  val output: Option[dataType] = None

  val uuid: UUID = UUID.randomUUID() // To keep set semantics working

  var prev: Set[Operation] = Set()// Derived from partial order
  var next: Set[Operation] = Set()

  def context(slave: Boolean = false): Set[Operation] = // All operations that happened before this one
    if prev.isEmpty then if slave then Set(this) else Set()
    else prev ++ prev.flatMap(o => o.context(true))

  def future(slave: Boolean = false): Set[Operation] = // All operations that happen after this one
    if next.isEmpty then if slave then Set(this) else Set()
    else next ++ next.flatMap(o => o.future(true))

  def v(): Boolean // Validity

  def s(): Boolean // Safety

  def l(): Boolean // Liveness

  override def toString: String = name  + input.mkString("(", ", ", ")") + "/" + output.mkString

  def legal(): Boolean = {
    println("!--- Verifying " + this)
    next.foreach(o => println(""+ this + "->" + o))
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