/** Function shape used by parsed V, S, and L predicates. */
type F = (Order, Operation) => Boolean

/** Creates custom operations from the user-defined DSL. */
class CustomOperationFactory(inputTypes: Array[DataTypes],
                             outputType: Option[DataTypes],
                             val name: String,
                             val vPred: F,
                             val sPred: F,
                             val lPred: F):

  /** Creates an operation instance with the declared signature and predicates. */
  def create(process: Process, obj: Object, args: List[DataType], ret: Option[DataType], start: Int, end: Int): Operation =
    new CustomOperation(process, obj, args, ret, start, end)

  /** The custom operation defined by the user in the DSL. */
  private class CustomOperation(process: Process, obj: Object, args: List[DataType], ret: Option[DataType], start: Int, end: Int)
      extends Operation(process, obj, inputTypes, outputType, start, end, name):

    override val input: Array[DataType] = args.toArray
    override val output: Option[DataType] = ret

    /** AMECOS validity predicate. */
    override def v(order: Order): Boolean = vPred(order, this)

    /** AMECOS safety predicate. */
    override def s(order: Order): Boolean = sPred(order, this)

    /** AMECOS liveness predicate. */
    override def l(order: Order): Boolean = lPred(order, this)
