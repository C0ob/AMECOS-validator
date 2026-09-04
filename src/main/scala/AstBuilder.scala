import Amecos.{AmecosBaseVisitor, AmecosParser}
import Ast.*

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.convert.ImplicitConversions.`map AsJavaMap`

class AstBuilder extends AmecosBaseVisitor[Ast]:

  private var objMap: Map[String, Crdt] = Map()
  private var ops: List[Operation] = List()
  private var order: Map[Operation, Set[Operation]] = Map()

  override def visitApp(ctx: AmecosParser.AppContext): App =
    objMap = ctx.init.asScala.map(visitInit).map(p => (p.name, p.crdt)).toMap
    println("[parser] Initialized objects: " + objMap.keys.mkString(","))

    ops = ctx.opex().asScala.map(visitOpex).map(_.operation).toList
    println("[parser] parsed " + ops.length + " op-exes")

    ctx.order().asScala.map(visitOrder).foreach(o =>
      if order.contains(o.first) then order.replace(o.first, order(o.first) + o.second)
      else order = order + Tuple2(o.first, Set(o.second))
    )
    App(objMap, ops, order)


  override def visitInit(ctx: AmecosParser.InitContext): InitObj =
    val name = ctx.OBJ().getText
    val crdt_type = ctx.NAME().getText
    InitObj(name, Crdt.new_crdt(crdt_type))

  override def visitArgs(ctx: AmecosParser.ArgsContext): Args = {
    if ctx == null then Args(List())
    else Args(ctx.NUM().asScala.toList.map(n => Integer.parseInt(n.toString)))
  }

  override def visitOpex(ctx: AmecosParser.OpexContext): Opex =
    val obj_name = ctx.OBJ().getText
    val op_name = ctx.NAME().getText
    val agrs = visitArgs(ctx.args()).args
    val ret = if ctx.NUM() == null then None else Some(Integer.parseInt(ctx.NUM().getText))
    val obj = objMap.getOrElse(obj_name, throw new Exception("Object " + obj_name + " not found: " + ctx.getText))
    Opex(obj.new_op(op_name, agrs, ret))

  override def visitOrder(ctx: AmecosParser.OrderContext): Order =
    val nums = ctx.NUM().asScala.map(n => Integer.parseInt(n.getText))
    try {
      Order(ops(nums.head), ops(nums(1)))
    } catch {
      case outOfBoundsException:
        Any => throw new IllegalArgumentException("Operation index invalid: " + ctx.getText)
    }

    