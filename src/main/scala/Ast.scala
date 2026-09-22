/** Nodes produced while translating the `.amecos` parse tree. */
trait Ast

enum PredType:
  /** Validity predicate. */
  case v, s, l
  
enum MathOps:
  /** Addition operator. */
  case plus, min, times, div

/** A type appearing in a DSL operation signature, before generic types are specialized. */
enum TypeExpr:
  case Concrete(value: DataTypes)
  case Variable(name: String)

  def resolve(types: Map[String, DataTypes]): DataTypes = this match
    case Concrete(value) => value
    case Variable(name) =>
      types.getOrElse(name, throw IllegalArgumentException(s"Unresolved type variable: $name"))
  
/** Constructors for the parser's intermediate representation. */
object Ast:
  /** Complete parsed application: objects, op-exes, ordering, and checks. */
  case class App(objs: Map[String, Object], history: History, order: Order, consistencies: Set[Consistency]) extends Ast
  
  /** Parsed custom operation. */
  case class TypeDef(factory: CustomObjectFactory) extends Ast
  
  /** Parsed custom operation. */
  case class OpDef(factory: CustomOperationFactory) extends Ast
  
  /** Parsed operation signature. */
  case class OpSignature(inputTypes: Array[TypeExpr], outputType: TypeExpr) extends Ast
  
  /** Parsed input signature types. */
  case class InType(inputTypes: Array[DataTypes]) extends Ast
  /** Parsed output signature type. */
  case class OutType(outputType: Option[DataTypes]) extends Ast
  
  /** Parsed v, s or l predicate. */
  case class Predicate(f: Formula, predType: PredType) extends Ast
  
  /** Parsed arithmetic operator. */
  case class MathOp(op: MathOps) extends Ast

  /** Parsed object initialization. */
  case class InitObj(name: String, obj: Object) extends Ast

  /** Parsed process and its op-exes. */
  case class Proc(process: Process, opExes: List[Operation]) extends Ast

  /** Parsed operation execution. */
  case class Opex(operation: Operation) extends Ast

  /** Parsed integer operation arguments. */
  case class Args(args: List[Data]) extends Ast

  /** Parsed partial-order edge between two op-exes. */
  case class OrderEdge(first: Operation, second: Operation) extends Ast

  /** Parsed inclusive invocation/response interval. */
  case class Interval(start: Int, end: Int) extends Ast

  /** Parsed reference to an op-ex by process and index. */
  case class OpexRef(operation: Operation) extends Ast

  /** Parsed set of requested consistency checks. */
  case class Consistencies(consistencies: Set[Consistency]) extends Ast
