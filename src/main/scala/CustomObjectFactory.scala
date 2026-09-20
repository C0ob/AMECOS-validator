/** Creates objects of custom types defined in the DSL. */
class CustomObjectFactory(val name: String, opFactories: Set[CustomOperationFactory]):
  private def factory_new_op(obj: Object, process: Process, name: String, args: List[Data], ret: Option[Data], start: Int, end: Int): Operation =
    val factory = opFactories.find(_.name == name)
    factory match
      case Some(operation) => operation.create(process, obj, args, ret, start, end)
      case None => throw new Exception("Operation not defined for " + this.name + " : " + name)

  /** Creates a runtime object instance backed by this type definition. */
  def create(): Object = new CustomObject()

  private class CustomObject extends Object(name):
    override def new_op(process: Process, name: String, args: List[Data], ret: Option[Data], start: Int, end: Int): Operation =
      factory_new_op(this, process, name, args, ret, start, end)
