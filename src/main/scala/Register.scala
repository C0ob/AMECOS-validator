class Register(name: String) extends Crdt(name):
  override def new_op(name: String, args: List[Int], ret: Option[Int]): Operation =
    name match
      case "Read" => new Read(this, args, ret)
      case "Write" => new Write(this, args, ret)
      case _ => super.new_op(name, args, ret)

class Read(obj: Crdt, in: List[Int], out: Option[Int]) extends Operation(obj, Array(), Some(DataType.Int), "Read"):
  if in.nonEmpty then throw new IllegalArgumentException("Read has 0 arguments, but got " + in.mkString(","))
  if out.isEmpty then throw new IllegalArgumentException("Read returns something, but got nothing")
  override val output: Option[Int] = out

  def v(): Boolean = context().exists(o => o.name == "Write" && o.obj == this.obj) // At least one write happened before

  def s(): Boolean = context().exists(o => // The output got written before and did not get overwritten
    o.name == "Write" &&
      o.input(0) == output.get &&
      o.obj == this.obj &&
      !context().intersect(o.future()).exists(_.name == "Write" && o.input(0) == output.get))

  def l(): Boolean = true // Assume operation completed at some point

class Write(obj: Crdt, in: List[Int], out: Option[Int]) extends Operation(obj, Array(DataType.Int), None, "Write"):
  if out.isDefined then throw new IllegalArgumentException("Write returns nothing, but got " + out)
  if in.size != 1 then throw new IllegalArgumentException("Write has 1 argument, but got " + in.mkString(","))
  override val input = Array(in.head)

  def v(): Boolean = true // Can always write

  def s(): Boolean = true // Is always safe to write

  def l(): Boolean = true // Assume operation completed at some point