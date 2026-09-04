sealed trait Ast

object Ast:
  case class App(objs: Map[String, Crdt], opexes: List[Operation], ordering: Map[Operation, Set[Operation]]) extends Ast

  case class InitObj(name: String, crdt: Crdt) extends Ast

  case class Opex(operation: Operation) extends Ast

  case class Args(args: List[Int]) extends Ast

  case class Order(first: Operation, second: Operation) extends Ast