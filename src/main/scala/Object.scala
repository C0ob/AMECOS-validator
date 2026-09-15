/** Base type for an AMECOS object specification. */
abstract class Object(val name: String):
  /** Constructs an operation execution belonging to this object. */
  def new_op(process: Process, name: String, args: List[Int], ret: Option[Int], start: Int, end: Int): Operation =
    throw new IllegalArgumentException("Operation not found")

/** Factory for the object specifications supported by the DSL. */
object Object:
  /** Creates a registered object implementation by its DSL type name. */
  def new_object(object_type: String, name: String): Object =
    object_type match
      case "Register" => new Register(name)
      case _ => throw new IllegalArgumentException("Object type not found")
