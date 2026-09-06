sealed trait Ast

object Ast:
  case class App(objs: Map[String, Crdt], opexes: List[Operation], ordering: Map[Operation, Set[Operation]], consistensies: Set[Consistency]) extends Ast

  case class InitObj(name: String, crdt: Crdt) extends Ast

  case class Proc(process: Process, opexes: List[Operation]) extends Ast

  case class Opex(operation: Operation) extends Ast

  case class Args(args: List[Int]) extends Ast

  case class Order(first: Operation, second: Operation) extends Ast
  
  case class Interval(start: Int, end: Int) extends Ast

  case class OpexRef(operation: Operation) extends Ast
  
  case class Consistencies(consistencies: Set[Consistency]) extends Ast