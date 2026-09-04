abstract class Crdt(val name: String):
  def new_op(name: String, args: List[Int], ret: Option[Int]): Operation =
    throw new IllegalArgumentException("Operation not found")

object Crdt:
  def new_crdt(name: String): Crdt =
    name match
      case "Register" => new Register(name)
      case _ => throw new IllegalArgumentException("Crdt type not found")
