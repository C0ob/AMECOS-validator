import Amecos.{AmecosBaseVisitor, AmecosParser}
import Ast.*

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.convert.ImplicitConversions.`map AsJavaMap`

/** Builds the application AST and domain objects from the generated ANTLR tree. */
class AstBuilder extends AmecosBaseVisitor[Ast]:

  private var objMap: Map[String, Crdt] = Map()
  private var processes: Set[Process] = Set()
  private var opexes: List[Operation] = List()
  private var order: Map[Operation, Set[Operation]] = Map()
  private var consistencies: Set[Consistency] = Set()

  /** Visits the complete `.amecos` application. */
  override def visitApp(ctx: AmecosParser.AppContext): App =
    if ctx.consistencies() != null then consistencies = visitConsistencies(ctx.consistencies()).consistencies
    println("[parser] Consistencies: " + consistencies.mkString(","))
    objMap = ctx.init.asScala.map(visitInit).map(p => (p.name, p.crdt)).toMap
    println("[parser] Initialized objects: " + objMap.keys.mkString(","))

    ctx.process().asScala.map(visitProcess).foreach( p =>
      processes = processes + p.process
      opexes = opexes ++ p.opexes
    )
    println("[parser] parsed " + opexes.length + " op-exes")

    ctx.order().asScala.map(visitOrder).foreach(o =>
      if order.contains(o.first) then order.replace(o.first, order(o.first) + o.second)
      else order = order + Tuple2(o.first, Set(o.second))
    )
    App(objMap, opexes, order, consistencies)

  /** Resolves consistency names from a `check` clause. */
  override def visitConsistencies(ctx: AmecosParser.ConsistenciesContext): Consistencies =
    Consistencies(ctx.NAME().asScala.map(n => Consistency.get_consistency(n.getText)).toSet)

  /** Creates an object from a `new` declaration. */
  override def visitInit(ctx: AmecosParser.InitContext): InitObj =
    val name = ctx.OBJ().getText
    val crdt_type = ctx.NAME().getText
    val crdt = Crdt.new_crdt(crdt_type, name)
    InitObj(name, crdt)

  /** Creates a process and all of its op-exes. */
  override def visitProcess(ctx: AmecosParser.ProcessContext): Proc =
    val process = Process(ctx.PROC().getText)
    val ops = ctx.opex().asScala.map(o => visitOpex(o, process).operation).toList
    Proc(process, ops)

  /** Converts optional numeric arguments to the AST representation. */
  override def visitArgs(ctx: AmecosParser.ArgsContext): Args =
    if ctx == null then Args(List())
    else Args(ctx.NUM().asScala.toList.map(n => Integer.parseInt(n.toString)))

  /** Converts an op-ex interval to an AST interval. */
  override def visitInterval(ctx: AmecosParser.IntervalContext): Interval =
    val nums = ctx.NUM().asScala.map(n => Integer.parseInt(n.getText)).toList
    Interval(nums.head, nums(1))

  /** Creates an object-specific operation execution. */
  def visitOpex(ctx: AmecosParser.OpexContext, process: Process): Opex =
    val obj_name = ctx.OBJ().getText
    val op_name = ctx.NAME().getText
    val args = visitArgs(ctx.args()).args
    val interval = visitInterval(ctx.interval())
    val ret = if ctx.NUM() == null then None else Some(Integer.parseInt(ctx.NUM().getText))
    val obj = objMap.getOrElse(obj_name, throw new Exception("Object " + obj_name + " not found: " + ctx.getText))
    try 
      Opex(obj.new_op(process, op_name, args, ret, interval.start, interval.end))
    catch 
      case e: Exception => throw new IllegalArgumentException(e.getMessage + ": " + ctx.getText)

  /** Resolves a process/index reference to a previously parsed op-ex. */
  override def visitOpexRef(ctx: AmecosParser.OpexRefContext): OpexRef =
    val process = Process(ctx.PROC().getText)
    try 
      OpexRef(opexes.filter(_.process == process)(Integer.parseInt(ctx.NUM().getText)))
    catch 
      case e: IndexOutOfBoundsException => throw new IllegalArgumentException("Invalid opex index: " + ctx.getText)


  /** Converts an ordering edge into an AST node. */
  override def visitOrder(ctx: AmecosParser.OrderContext): Order = {
    val ops = ctx.opexRef().asScala.map(o => visitOpexRef(o).operation).toList
    Order(ops.head, ops(1))

  }
