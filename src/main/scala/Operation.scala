type dataType = Integer | Boolean | String

enum DataType:
  case Int, Boolean, String

// Represents an operation. Objects of this class are op-exes.
abstract class Operation(val inputTypes: Array[DataType],
                         val outputType: Option[DataType], val name: String = "") {

  val input: Array[dataType] = Array()
  val output: Option[dataType] = None

  var prev: Set[Operation] = Set()// Derived from partial order
  var next: Set[Operation] = Set()

  def context(): Set[Operation] =
    if prev.isEmpty then Set(this)
    else prev ++ prev.flatMap(o => o.context())

  def future(): Set[Operation] =
    if next.isEmpty then Set(this)
    else next ++ next.flatMap(o => o.future())

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
    resV && resS && resL
  }
}

// Example: shared register
class Read(data: Integer) extends Operation(Array(), Some(DataType.Int), "Read"):
  override val output = Some(data)
  def v(): Boolean = context().exists(_.name == "Write") // At least one write happened before
  def s(): Boolean = context().exists(o => // The output got written before and did not get overwritten
    o.name == "Write" &&
    o.input(0) == output.get &&
      !context().intersect(o.future()).excl(o).exists(_.name == "Write" && o.input(0) == output.get))

  def l(): Boolean = true // Assume operation completed at some point

class Write(data: Integer) extends Operation(Array(DataType.Int), None, "Write"):
  override val input = Array(data)
  def v(): Boolean = true // Can always write
  def s(): Boolean = true // Is always safe to write
  def l(): Boolean = true // Assume operation completed at some point