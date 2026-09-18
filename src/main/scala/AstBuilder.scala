import Amecos.{AmecosBaseVisitor, AmecosParser}
import Ast.*

import scala.jdk.CollectionConverters.*

/** Builds the application AST and domain objects from the generated ANTLR tree. */
/** Converts generated ANTLR parse trees into the application's AST and model objects. */
class AstBuilder extends AmecosBaseVisitor[Ast]:

  var opExes: List[Operation] = List()
  var history: History = null // could have been done more cleanly, I know :p
  var order: Order = null
  private var objMap: Map[String, Object] = Map()
  private var processes: Set[Process] = Set()
  private var consistencies: Set[Consistency] = Set()

  /** Visits the complete `.amecos` application. */
  override def visitApp(ctx: AmecosParser.AppContext): App =
    if ctx.typedef() != null then ctx.typedef().asScala.foreach(visitTypedef)
    if ctx.consistencies() != null then consistencies = visitConsistencies(ctx.consistencies()).consistencies
    println("[parser] Consistencies: " + consistencies.mkString(","))
    objMap = ctx.init.asScala.map(visitInit).map(p => (p.name, p.obj)).toMap
    println("[parser] Initialized objects: " + objMap.keys.mkString(","))

    ctx.process().asScala.map(visitProcess).foreach(p =>
      processes = processes + p.process
      opExes = opExes ++ p.opExes
    )

    history = History(opExes.toSet)
    println("[parser] parsed " + opExes.size + " op-exes")
    order = Order(history)

    ctx.order().asScala.map(visitOrder).foreach(o =>
      order.add(o.first, o.second)
    )
    App(objMap, history, order, consistencies)

  /** Creates new object factories from a `typedef` declaration. */
  override def visitTypedef(ctx: AmecosParser.TypedefContext): TypeDef =
    val typeName = ctx.NAME().getText
    val opFactories = ctx.opdef().asScala.map(o => visitOpdef(o).factory).toSet
    TypeDef(new CustomObjectFactory(typeName, opFactories))

  /** Creates new operation factories from a `typedef` type declaration. */
  override def visitOpdef(ctx: AmecosParser.OpdefContext): OpDef =
    val name = ctx.NAME().getText
    val signature = visitSignature(ctx.signature())

    // placeholders
    var vPred: F = (_, _) => true
    var sPred: F = (_, _) => true
    var lPred: F = (_, _) => true

    if ctx.predicate() != null then ctx.predicate().forEach(p =>
      val predicate = visitPredicate(p)
      predicate.predType match {
        case PredType.v => vPred = predicate.f
        case PredType.s => sPred = predicate.f
        case PredType.l => lPred = predicate.f
      })
    OpDef(CustomOperationFactory(signature.inputTypes, signature.outputType, name, vPred, sPred, lPred))

  override def visitSignature(ctx: AmecosParser.SignatureContext): OpSignature =
    val inputTypes = visitInputtype(ctx.inputtype()).inputTypes
    val outputTypes = visitOutputtype(ctx.outputtype()).outputType
    OpSignature(inputTypes, outputTypes)

  override def visitInputtype(ctx: AmecosParser.InputtypeContext): InType =
    if ctx.getText == "void" then return InType(Array())
    if ctx.getText.contains("void") then throw IllegalArgumentException(s"void cannot be part of composite signature: " + ctx.getText)
    val length = ctx.getText.count(c => c != ' ' || c != ',') / 3 // BREAKS IF MORE TYPES ARE ADDED!
    InType(Array.fill(length)(DataType.Int))


  override def visitOutputtype(ctx: AmecosParser.OutputtypeContext): OutType =
    if ctx.getText == "int" then OutType(Some(DataType.Int)) else OutType(None)

  override def visitPredicate(ctx: AmecosParser.PredicateContext): Predicate =
    val predType = ctx.getStart.getText match {
      case "V" => PredType.v
      case "S" => PredType.s
      case "L" => PredType.l
      case other => throw new IllegalArgumentException(s"Unknown predicate type: $other") // realistically never happens
    }

    Predicate((order, operation) => visitFormula(ctx.formula(), order, operation, operation).f(order, operation), predType)

  def visitFormula(ctx: AmecosParser.FormulaContext, order: Order, operation: Operation, ths: Operation): Formula =
    visitDisjunction(ctx.disjunction(), order, operation, ths)

  def visitDisjunction(ctx: AmecosParser.DisjunctionContext, order: Order, operation: Operation, ths: Operation): Formula =
    val formulas = ctx.conjunction().asScala.map(c => visitConjunction(c, order, operation, ths).f).toList
    Formula((_, _) => formulas.exists(f => f(order, operation)))

  def visitConjunction(ctx: AmecosParser.ConjunctionContext, order: Order, operation: Operation, ths: Operation): Formula =
    val formulas = ctx.unary().asScala.map(u => visitUnary(u, order, operation, ths).f).toList
    Formula((_, _) => formulas.forall(f => f(order, operation)))

  def visitUnary(ctx: AmecosParser.UnaryContext, order: Order, operation: Operation, ths: Operation): Formula =
    if ctx.NOT() != null then Formula((_, _) => !visitUnary(ctx.unary(), order, operation, ths).f(order, operation))
    else Formula((_, _) => visitFormulaAtom(ctx.formulaAtom(), order, operation, ths).f(order, operation))

  def visitFormulaAtom(ctx: AmecosParser.FormulaAtomContext, order: Order, operation: Operation, ths: Operation): Formula =
    if ctx.TRUE() != null then return Formula((_, _) => true)
    if ctx.FALSE() != null then return Formula((_, _) => false)
    if ctx.formula() != null then return visitFormula(ctx.formula(), order, operation, ths)
    if ctx.quantifier() != null then return Formula((order, operation) => visitQuantifier(ctx.quantifier(), order, operation, ths).f(order, operation))

    val a = visitValueExpr(ctx.valueExpr(0), order, operation, ths)
    val b = visitValueExpr(ctx.valueExpr(1), order, operation, ths)
    if a.valType == ValTypes.operation && b.valType == ValTypes.operation then {
      val comp: (Operation, Operation) => Boolean = visitComparator(ctx.comparator()).comparator match {
        case ComparatorType.eq => (a, b) => a == b
        case ComparatorType.neq => (a, b) => a != b
        case ComparatorType.sameObj => (a, b) => a.obj == b.obj
        case ComparatorType.sameInput => (a, b) => a.input sameElements b.input
        case ComparatorType.sameOutput => (a, b) => a.output == b.output
        case other => throw new IllegalArgumentException(s"Unknown comparator for ${a.valType}: $other")
      }
      return Formula((_, _) => comp(a.opVal, b.opVal))
    }
    if a.valType == ValTypes.string && b.valType == ValTypes.string then {
      val comp: (String, String) => Boolean = visitComparator(ctx.comparator()).comparator match {
        case ComparatorType.eq => (a, b) => a == b
        case ComparatorType.neq => (a, b) => a != b
        case other => throw new IllegalArgumentException(s"Unknown comparator for ${a.valType}: $other")
      }
      return Formula((_, _) => comp(a.stringVal, b.stringVal))
    }
    if a.valType != ValTypes.int || b.valType != ValTypes.int then throw IllegalArgumentException(s"Cannot invoke comparator on ${a.valType} and ${b.valType}")
    val comp: (dataType, dataType) => Boolean = visitComparator(ctx.comparator()).comparator match {
      case ComparatorType.eq => (a, b) => a == b
      case ComparatorType.neq => (a, b) => a != b
      case ComparatorType.leq => (a, b) => a <= b
      case ComparatorType.geq => (a, b) => a >= b
      case ComparatorType.lt => (a, b) => a < b
      case ComparatorType.gt => (a, b) => a > b
      case other => throw new IllegalArgumentException(s"Unknown comparator for ${a.valType}: $other")
    }
    Formula((_, _) => comp(a.intVal, b.intVal))


  def visitQuantifier(ctx: AmecosParser.QuantifierContext, order: Order, operation: Operation, ths: Operation): Formula =
    val set = visitSetExpr(ctx.setExpr(), order, operation, ths)
    if set.valType != ValTypes.operation then throw new IllegalArgumentException("Cannot quantify over non-operation set: " + set.valType)
    var opSet = set.opVals
    if ctx.where() != null then opSet = opSet.filter(o => visitWhere(ctx.where(), order, o, operation).f(order, o))
    if ctx.FORALL() != null then Formula((_, _) => opSet.forall(o => visitFormula(ctx.formula(), order, o, operation).f(order, o)))
      else Formula((_, _) => opSet.forall(o => visitFormula(ctx.formula(), order, o, operation).f(order, o)))

  def visitWhere(ctx: AmecosParser.WhereContext, order: Order, operation: Operation, ths: Operation): Formula = 
    visitFormula(ctx.formula(), order, operation, ths)
    
  override def visitComparator(ctx: AmecosParser.ComparatorContext): Comparator =
    ctx.getText match {
      case "==" => Comparator(ComparatorType.eq)
      case "!=" => Comparator(ComparatorType.neq)
      case "<=" => Comparator(ComparatorType.leq)
      case ">=" => Comparator(ComparatorType.geq)
      case "<" => Comparator(ComparatorType.lt)
      case ">" => Comparator(ComparatorType.gt)
      case "same object as" => Comparator(ComparatorType.sameObj)
      case "same input as" => Comparator(ComparatorType.sameInput)
      case "same output as" => Comparator(ComparatorType.sameOutput)
      case other => throw new IllegalArgumentException(s"Unknown comparator: $other")
    }

  /** Evaluates a parsed value expression in the context of an operation and order. */
  def visitValueExpr(ctx: AmecosParser.ValueExprContext, order: Order, operation: Operation, ths: Operation): Value = {
    if ctx.valueAtom() != null then return visitValueAtom(ctx.valueAtom(), order, operation, ths)
    if ctx.mathOp() != null then {
      val op = visitMathOp(ctx.mathOp()).op match {
        case MathOps.plus => (a: dataType, b: dataType) => a + b
        case MathOps.min => (a: dataType, b: dataType) => a - b
        case MathOps.times => (a: dataType, b: dataType) => a * b
        case MathOps.div => (a: dataType, b: dataType) => a / b
      }
      val a = visitValueExpr(ctx.valueExpr(0), order, operation, ths)
      val b = visitValueExpr(ctx.valueExpr(1), order, operation, ths)
       if a.valType != ValTypes.int || b.valType != ValTypes.int then throw IllegalArgumentException(s"Cannot invoke arithmetic on ${a.valType} and ${b.valType}")
       return Value(ValTypes.int, op(a.intVal, b.intVal))
    }
    if ctx.index() != null then return visitIndex(ctx.index(), order, operation, ths)
    val set = visitSetExpr(ctx.setExpr(), order, operation, ths)
    if set.valType != ValTypes.operation then throw IllegalArgumentException(s"Cannot get latest of set type ${set.valType}")
    Value(ValTypes.int, Math.max(set.intVals.length, set.opVals.size))
  }

  def visitIndex(ctx: AmecosParser.IndexContext, order: Order, operation: Operation, tht: Operation ): Value =
    val set = visitSetExpr(ctx.setExpr(), order, operation, tht)
    if set.valType != ValTypes.int then throw IllegalArgumentException(s"Cannot index on type ${set.valType}.")
    val idx = Integer.parseInt(ctx.NUM().getText)
    if idx >= set.intVals.length then throw IllegalArgumentException(s"Index out of bounds at ${ctx.getText}")
    Value(ValTypes.int, set.intVals(idx))

  /** Evaluates a literal or operation-output value. */
  def visitValueAtom(ctx: AmecosParser.ValueAtomContext, order: Order, operation: Operation, ths: Operation): Value =
    if ctx.NUM() != null then return Value(ValTypes.int, Integer.parseInt(ctx.getText))
    if ctx.operationField() != null then {
      val target = if ctx.getText.startsWith("this") then ths else operation
      if ctx.getText.endsWith("name") then return Value(ValTypes.string, stringVal = target.name)
      return Value(ValTypes.int, target.output.getOrElse(throw new Exception("No output for " + target.name)))
    }
    if ctx.getText == "name" then return Value(ValTypes.string, stringVal = operation.name)
    if ctx.STRING() != null then return Value(ValTypes.string, stringVal = unquote(ctx.getText))
    if ctx.THIS() != null then return Value(ValTypes.operation, opVal = ths)
    if ctx.THAT() != null then return Value(ValTypes.operation, opVal = operation)
    Value(ValTypes.int, operation.output.getOrElse(throw new Exception("No output for " + operation.name)))

  private def unquote(value: String): String =
    value.substring(1, value.length - 1)
      .replace("\\\\", "\\")
      .replace("\\\"", "\"")

  override def visitMathOp(ctx: AmecosParser.MathOpContext): MathOp =
    ctx.getText match {
      case "+" => MathOp(MathOps.plus)
      case "-" => MathOp(MathOps.min)
      case "*" => MathOp(MathOps.times)
      case "/" => MathOp(MathOps.div)
      case other => throw new IllegalArgumentException(s"Unknown math operator: $other")
    }

  /** Evaluates a context, future, or input set expression. */
  def visitSetExpr(ctx: AmecosParser.SetExprContext, order: Order, operation: Operation, ths: Operation): SetVal =
    val op = if ctx.THIS() != null then ths else operation
    visitSetAtom(ctx.setAtom(), order, op)

  def visitSetAtom(ctx: AmecosParser.SetAtomContext, order: Order, operation: Operation): SetVal =
    ctx.getText match {
      case "context" => SetVal(ValTypes.operation, Array(), order.context(operation))
      case "future" => SetVal(ValTypes.operation, Array(), order.future(operation))
      case "input" => SetVal(ValTypes.int, operation.input)
      case "all" => SetVal(ValTypes.operation, Array(), order.history.opExes)
      case other => throw new IllegalArgumentException(s"Unknown set expression: $other")
  }

  /** Resolves consistency names from a `check` clause. */
  override def visitConsistencies(ctx: AmecosParser.ConsistenciesContext): Consistencies =
    Consistencies(ctx.NAME().asScala.map(n => Consistency.get_consistency(n.getText)).toSet)

  /** Creates an object from a `new` declaration. */
  override def visitInit(ctx: AmecosParser.InitContext): InitObj =
    val name = ctx.OBJ().getText
    val object_type = ctx.NAME().getText
    val obj = Object.new_object(object_type, name)
    InitObj(name, obj)

  /** Creates a process and all of its op-exes. */
  override def visitProcess(ctx: AmecosParser.ProcessContext): Proc =
    val process = Process(ctx.PROC().getText)
    val ops = ctx.opex().asScala.map(o => visitOpex(o, process).operation).toList
    Proc(process, ops)

  /** Creates an object-specific operation execution. */
  def visitOpex(ctx: AmecosParser.OpexContext, process: Process): Opex =
    val obj_name = ctx.OBJ().getText
    val op_name = ctx.NAME().getText
    val args = visitArgs(ctx.args()).args
    val interval = if ctx.interval() == null then Interval(0, 0) else visitInterval(ctx.interval())
    val ret = if ctx.NUM() == null then None else Some(Integer.parseInt(ctx.NUM().getText))
    val obj = objMap.getOrElse(obj_name, throw new Exception("Object " + obj_name + " not found: " + ctx.getText))
    try
      Opex(obj.new_op(process, op_name, args, ret, interval.start, interval.end))
    catch
      case e: Exception => throw new IllegalArgumentException(e.getMessage + ": " + ctx.getText)

  /** Converts optional numeric arguments to the AST representation. */
  override def visitArgs(ctx: AmecosParser.ArgsContext): Args =
    if ctx == null then Args(List())
    else Args(ctx.NUM().asScala.toList.map(n => Integer.parseInt(n.toString)))

  /** Converts an op-ex interval to an AST interval. */
  override def visitInterval(ctx: AmecosParser.IntervalContext): Interval =
    val nums = ctx.NUM().asScala.map(n => Integer.parseInt(n.getText)).toList
    Interval(nums.head, nums(1))

  /** Converts an ordering edge into an AST node. */
  override def visitOrder(ctx: AmecosParser.OrderContext): OrderEdge = {
    val ops = ctx.opexRef().asScala.map(o => visitOpexRef(o).operation).toList
    OrderEdge(ops.head, ops(1))

  }

  /** Resolves a process/index reference to a previously parsed op-ex. */
  override def visitOpexRef(ctx: AmecosParser.OpexRefContext): OpexRef =
    val process = Process(ctx.PROC().getText)
    try
      OpexRef(opExes.filter(_.process == process)(Integer.parseInt(ctx.NUM().getText)))
    catch
      case e: IndexOutOfBoundsException => throw new IllegalArgumentException("Invalid opex index: " + ctx.getText)
