/** Nodes produced while translating the `.amecos` parse tree. */
sealed trait Ast

enum PredType:
  /** Validity predicate. */
  case v, s, l
  
enum ComparatorType:
  /** Equality comparator. */
  case eq, neq, lt, leq, gt, geq
  
enum MathOps:
  /** Addition operator. */
  case plus, min, times, div
  
enum ValTypes:
  /** Integer value. */
  case int, operation
  
/** Constructors for the parser's intermediate representation. */
object Ast:
  /** Complete parsed application: objects, op-exes, ordering, and checks. */
  case class App(objs: Map[String, Object], history: History, order: Order, consistencies: Set[Consistency]) extends Ast
  
  /** Parsed custom operation. */
  case class TypeDef(factory: CustomObjectFactory) extends Ast
  
  /** Parsed custom operation. */
  case class OpDef(factory: CustomOperationFactory) extends Ast
  
  /** Parsed operation signature. */
  case class OpSignature(inputTypes: Array[DataType], outputType: Option[DataType]) extends Ast
  
  /** Parsed input signature types. */
  case class InType(inputTypes: Array[DataType]) extends Ast
  /** Parsed output signature type. */
  case class OutType(outputType: Option[DataType]) extends Ast
  
  /** Parsed v, s or l predicate. */
  case class Predicate(f: F, predType: PredType) extends Ast
  
  /** Parsed formula for v, s, and l predicates. */
  case class Formula(f: F) extends Ast
  
  /** Parsed scalar or operation value. */
  case class Value(valType: ValTypes, intVal: Int = 0, opVal: Operation = null) extends Ast
  
  /** Parsed comparison operator. */
  case class Comparator(comparator: ComparatorType) extends Ast
  
  /** Parsed arithmetic operator. */
  case class MathOp(op: MathOps) extends Ast
  
  case class SetVal(valType: ValTypes, intVals: Array[Int] = Array(), opVals: Set[Operation] = Set()) extends Ast

  /** Parsed object initialization. */
  case class InitObj(name: String, obj: Object) extends Ast

  /** Parsed process and its op-exes. */
  case class Proc(process: Process, opExes: List[Operation]) extends Ast

  /** Parsed operation execution. */
  case class Opex(operation: Operation) extends Ast

  /** Parsed integer operation arguments. */
  case class Args(args: List[Int]) extends Ast

  /** Parsed partial-order edge between two op-exes. */
  case class OrderEdge(first: Operation, second: Operation) extends Ast

  /** Parsed inclusive invocation/response interval. */
  case class Interval(start: Int, end: Int) extends Ast

  /** Parsed reference to an op-ex by process and index. */
  case class OpexRef(operation: Operation) extends Ast

  /** Parsed set of requested consistency checks. */
  case class Consistencies(consistencies: Set[Consistency]) extends Ast
