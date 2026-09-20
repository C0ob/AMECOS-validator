/** Shared register specification used by the AMECOS examples. */
class Register(name: String) extends Object(name):
  /** Creates a register read or write op-ex. */
  override def new_op(process: Process, name: String, args: List[Data], ret: Option[Data], start: Int, end: Int): Operation =
    name match
      case "Read" => new Read(process, this, args, ret, start, end)
      case "Write" => new Write(process, this, args, ret, start, end)
      case _ => super.new_op(process, name, args, ret, start, end)

/** Register read operation; its return value must be the current register value. */
class Read(process: Process, obj: Object, in: List[Data], out: Option[Data], start: Int, end: Int) extends Operation(process, obj, Array.empty, Some(DataTypes.Int), start, end, "Read"):
  if in.nonEmpty then throw new IllegalArgumentException("Read has 0 arguments, but got " + in.mkString(","))
  if out.isEmpty then throw new IllegalArgumentException("Read returns something, but got nothing")
  override val output: Data = out.get

  /** A read is valid once a write to the register precedes it. */
  def v(order: Order): Boolean = context(order).exists(o => o.name == "Write" && o.obj == this.obj)

  /** A read is safe when its returned value was the latest preceding write. */
  def s(order: Order): Boolean = context(order).exists(o =>
    o.name == "Write" &&
      o.input(0) == output &&
      o.obj == this.obj &&
       !context(order).intersect(o.future(order)).exists(p => p.name == "Write" && p.input(0) != output && p.obj == this.obj))

  /** Register operations are assumed to complete. */
  def l(order: Order): Boolean = true

/** Register write operation. */
class Write(process: Process, obj: Object, in: List[Data], out: Option[Data], start: Int, end: Int) extends Operation(process, obj, Array(DataTypes.Int), None, start, end, "Write"):
  if out.isDefined then throw new IllegalArgumentException("Write returns nothing, but got " + out)
  if in.size != 1 then throw new IllegalArgumentException("Write has 1 argument, but got " + in.mkString(","))
  override val input: Array[Data] = Array(in.head)

  /** A write is always valid. */
  def v(order: Order): Boolean = true

  /** A write is always safe. */
  def s(order: Order): Boolean = true

  /** Register operations are assumed to complete. */
  def l(order: Order): Boolean = true
