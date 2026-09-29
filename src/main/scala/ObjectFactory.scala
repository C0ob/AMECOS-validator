/** Creates objects of custom types defined in the DSL. */
class ObjectFactory(val name: String, opFactories: Set[CustomOperationFactory], typeVars: List[String]):
  require(typeVars.distinct.size == typeVars.size, s"Duplicate type parameter in $name")
  private def factory_new_op(obj: Object, process: Process, name: String, args: List[Data], ret: Option[Data], start: Int, end: Int,
                             factories: Set[CustomOperationFactory]): Operation =
    val factory = factories.find(_.name == name)
    factory match
      case Some(operationFactory) => operationFactory.create(process, obj, args, ret, start, end)
      case None => throw new Exception("Operation not defined for " + this.name + " : " + name)

  /** Creates a runtime object instance backed by this type definition. */
  def create(objectName: String, typeArgs: List[DataTypes]): Object =
    if typeArgs.size != typeVars.size then
      throw IllegalArgumentException(
        s"Type $name expects ${typeVars.size} type argument(s), got ${typeArgs.size}")
    val typeVals = typeVars.zip(typeArgs).toMap
    new CustomObject(objectName, opFactories.map(_.specialize(typeVals)))

  private class CustomObject(objectName: String, factories: Set[CustomOperationFactory]) extends Object(objectName):
    override def new_op(process: Process, name: String, args: List[Data], ret: Option[Data], start: Int, end: Int): Operation =
      factory_new_op(this, process, name, args, ret, start, end, factories)
