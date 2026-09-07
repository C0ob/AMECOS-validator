/** Base type for an AMECOS object specification. */
abstract class Crdt(val name: String):
  /** Constructs an operation execution belonging to this object. */
  def new_op(process: Process, name: String, args: List[Int], ret: Option[Int], start: Int, end: Int): Operation =
    throw new IllegalArgumentException("Operation not found")

/** Factory for the CRDT specifications supported by the DSL. */
object Crdt:
  /** Creates a registered CRDT implementation by its DSL type name. */
  def new_crdt(name: String): Crdt =
    name match
      case "Register" => new Register(name)
      case _ => throw new IllegalArgumentException("Crdt type not found")
