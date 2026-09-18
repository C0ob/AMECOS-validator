/** Base type for an AMECOS object specification. */
abstract class Object(val name: String):
  /** Constructs an operation execution belonging to this object. */
  def new_op(process: Process, name: String, args: List[DataType], ret: Option[DataType], start: Int, end: Int): Operation =
    throw new IllegalArgumentException("Operation not found for " + this.name + " : " + name)

/** Factory for the object specifications supported by the DSL. */
object Object:
  /** Creates a registered object implementation by its DSL type name. */
  def new_object(object_type: String, name: String, factories: Map[String, CustomObjectFactory]): Object = {
    val customObjectFactory = factories.get(object_type)
    if customObjectFactory.isEmpty then
      object_type match
        case "Register" => new Register(name)
        case _ => throw new IllegalArgumentException("Object type not found: " + object_type)
    else customObjectFactory.get.create()    
  }
