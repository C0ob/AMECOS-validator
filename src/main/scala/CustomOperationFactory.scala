/** Function shape used by parsed V, S, and L predicates. */
type Formula = (Order, Operation) => Boolean

/** Creates custom operations from the user-defined DSL. */
class CustomOperationFactory(inputTypes: Array[TypeExpr],
                             outputType: TypeExpr,
                             val name: String,
                             val vPred: Formula,
                             val sPred: Formula,
                             val lPred: Formula):

  /** Specializes this operation signature for one concrete generic instantiation. */
  def specialize(typeVals: Map[String, DataTypes]): CustomOperationFactory =
    new CustomOperationFactory(
      inputTypes.map(t => TypeExpr.Concrete(t.resolve(typeVals))),
      TypeExpr.Concrete(outputType.resolve(typeVals)),
      name, vPred, sPred, lPred)

  /** Creates an operation instance with the declared signature and predicates. */
  def create(process: Process, obj: Object, args: List[Data], ret: Option[Data], start: Int, end: Int): Operation =
    new CustomOperation(process, obj, args, ret, start, end,
      inputTypes.map(_.resolve(Map.empty)), outputType.resolve(Map.empty))

  /** The custom operation defined by the user in the DSL. */
  private class CustomOperation(process: Process, obj: Object, args: List[Data], ret: Option[Data], start: Int, end: Int,
                                resolvedInputTypes: Array[DataTypes], resolvedOutputType: DataTypes)
      extends Operation(process, obj, resolvedInputTypes, resolvedOutputType, start, end, name):

    override val input: Array[Data] = args.toArray
    override val output: Data = ret.getOrElse(UnitData())

    if input.indices.exists(i => resolvedInputTypes(i) != input(i).getType) then throw IllegalArgumentException("Input type mismatch")
    if output.getType != resolvedOutputType then throw IllegalArgumentException("Output type mismatch")

    /** AMECOS validity predicate. */
    override def v(order: Order): Boolean = vPred(order, this)

    /** AMECOS safety predicate. */
    override def s(order: Order): Boolean = sPred(order, this)

    /** AMECOS liveness predicate. */
    override def l(order: Order): Boolean = lPred(order, this)
