/** Base type for an AMECOS object specification. */
abstract class Object(val name: String):
  /** Constructs an operation execution belonging to this object. */
  def new_op(process: Process, name: String, args: List[Data], ret: Option[Data], start: Int, end: Int): Operation =
    throw new IllegalArgumentException("Operation not found for " + this.name + " : " + name)

/** Factory for the object specifications supported by the DSL. */
object Object:
  /** Creates an object implementation by its DSL type name. */
  def new_object(object_type: String, name: String, factories: Map[String, ObjectFactory],
                 typeArgs: List[DataTypes] = Nil): Object = {
    val customObjectFactory = factories.get(object_type)
    if customObjectFactory.isEmpty then
      throw new IllegalArgumentException("Object type not found: " + object_type)
    else customObjectFactory.get.create(name, typeArgs)
  }
