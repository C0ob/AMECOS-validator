/** Nodes produced while translating the `.amecos` parse tree. */
sealed trait Ast

/** Constructors for the parser's intermediate representation. */
object Ast:
  /** Complete parsed application: objects, op-exes, ordering, and checks. */
  case class App(objs: Map[String, Object], history: History, order: Order, consistensies: Set[Consistency]) extends Ast

  /** Parsed object initialization. */
  case class InitObj(name: String, obj: Object) extends Ast

  /** Parsed process and its op-exes. */
  case class Proc(process: Process, opexes: List[Operation]) extends Ast

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
