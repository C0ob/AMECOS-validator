class Register(name: String) extends Crdt(name):
  override def new_op(process: Process, name: String, args: List[Int], ret: Option[Int], start: Int, end: Int): Operation =
    name match
      case "Read" => new Read(process, this, args, ret, start, end)
      case "Write" => new Write(process, this, args, ret, start, end)
      case _ => super.new_op(process, name, args, ret, start, end)

class Read(process: Process, obj: Crdt, in: List[Int], out: Option[Int], start: Int, end: Int) extends Operation(process, obj, Array(), Some(DataType.Int), start, end, "Read"):
  if in.nonEmpty then throw new IllegalArgumentException("Read has 0 arguments, but got " + in.mkString(","))
  if out.isEmpty then throw new IllegalArgumentException("Read returns something, but got nothing")
  override val output: Option[Int] = out

  def v(): Boolean = context().exists(o => o.name == "Write" && o.obj == this.obj) // At least one write happened before

  def s(): Boolean = context().exists(o => // The output got written before and did not get overwritten
    o.name == "Write" &&
      o.input(0) == output.get &&
      o.obj == this.obj &&
      !context().intersect(o.future()).exists(p => p.name == "Write" && p.input(0) == output.get && p.obj == this.obj))

  def l(): Boolean = true // Assume operation completed at some point

class Write(process: Process, obj: Crdt, in: List[Int], out: Option[Int], start: Int, end: Int) extends Operation(process, obj, Array(DataType.Int), None, start, end, "Write"):
  if out.isDefined then throw new IllegalArgumentException("Write returns nothing, but got " + out)
  if in.size != 1 then throw new IllegalArgumentException("Write has 1 argument, but got " + in.mkString(","))
  override val input = Array(in.head)

  def v(): Boolean = true // Can always write

  def s(): Boolean = true // Is always safe to write

  def l(): Boolean = true // Assume operation completed at some point