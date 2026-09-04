class Register(name: String) extends Crdt(name):
  override def new_op(name: String, args: List[Int], ret: Option[Int]): Operation =
    name match
      case "Read" => new Read(args, ret)
      case "Write" => new Write(args, ret)
      case _ => super.new_op(name, args, ret)

class Read(unused: List[Int], data: Option[Int]) extends Operation(Array(), Some(DataType.Int), "Read"):
  if unused.nonEmpty then throw new IllegalArgumentException("Read has 0 arguments, but got " + unused.mkString(","))
  override val output: Option[Int] = data
  def v(): Boolean = context().exists(_.name == "Write") // At least one write happened before
  def s(): Boolean = context().exists(o => // The output got written before and did not get overwritten
    o.name == "Write" &&
      o.input(0) == output.get &&
      !context().intersect(o.future()).exists(_.name == "Write" && o.input(0) == output.get))

  def l(): Boolean = true // Assume operation completed at some point

class Write(data: List[Int], unused: Option[Int]) extends Operation(Array(DataType.Int), None, "Write"):
  if unused.isDefined then throw new IllegalArgumentException("Write returns nothing, but got " + unused)
  override val input = Array(data.head)
  def v(): Boolean = true // Can always write
  def s(): Boolean = true // Is always safe to write
  def l(): Boolean = true // Assume operation completed at some point