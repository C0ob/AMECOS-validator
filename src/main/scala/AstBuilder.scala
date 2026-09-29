import Amecos.{AmecosBaseVisitor, AmecosParser}
import Ast.*

import scala.jdk.CollectionConverters.*

/** Builds the application AST and domain objects from the generated ANTLR tree. */
/** Converts generated ANTLR parse trees into the application's AST and model objects. */
class AstBuilder extends AmecosBaseVisitor[Ast]:

  var opExes: List[Operation] = List()
  var history: History = null // could have been done more cleanly, I know :p
  var order: Order = null
  private var factories = Map[String, ObjectFactory]()
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
    val typeVars =
      if ctx.typePars() == null then Nil
      else ctx.typePars().getText.drop(1).dropRight(1).split(",").toList
    val opFactories = ctx.opdef().asScala.map(o => visitOpdef(o).factory).toSet
    val factory = new ObjectFactory(typeName, opFactories, typeVars)
    factories = factories + (typeName -> factory)
    TypeDef(factory)

  /** Creates new operation factories from a `typedef` type declaration. */
  override def visitOpdef(ctx: AmecosParser.OpdefContext): OpDef =
    val name = ctx.NAME().getText
    val signature = visitSignature(ctx.signature())

    // placeholders
    var vPred: Formula = (_, _) => true
    var sPred: Formula = (_, _) => true
    var lPred: Formula = (_, _) => true

    if ctx.predicate() != null then ctx.predicate().forEach(p =>
      val predicate = visitPredicate(p)
      predicate.predType match {
        case PredType.v => vPred = predicate.f
        case PredType.s => sPred = predicate.f
        case PredType.l => lPred = predicate.f
      })
    OpDef(CustomOperationFactory(signature.inputTypes, signature.outputType, name, vPred, sPred, lPred))

  override def visitSignature(ctx: AmecosParser.SignatureContext): OpSignature =
    val inputTypes = parseInputtype(ctx.inputtype())
    val outputTypes = parseDatatype(ctx.datatype())
    OpSignature(inputTypes, outputTypes)

  private def parseInputtype(ctx: AmecosParser.InputtypeContext): Array[TypeExpr] =
    if ctx.getText == "void" then return Array()
    if ctx.getText.contains("void") then throw IllegalArgumentException(s"void cannot be part of composite signature: " + ctx.getText)
    ctx.datatype().asScala.map(parseDatatype).toArray


  private def parseDatatype(ctx: AmecosParser.DatatypeContext): TypeExpr =
    ctx.getText match
      case "int" => TypeExpr.Concrete(DataTypes.Int)
      case "string" => TypeExpr.Concrete(DataTypes.String)
      case "void" => TypeExpr.Concrete(DataTypes.Unit)
      case name => TypeExpr.Variable(name)

  override def visitPredicate(ctx: AmecosParser.PredicateContext): Predicate =
    val predType = ctx.getStart.getText match {
      case "V" => PredType.v
      case "S" => PredType.s
      case "L" => PredType.l
      case other => throw new IllegalArgumentException(s"Unknown predicate type: $other") // realistically never happens
    }

    Predicate((order, operation) => visitFormula(ctx.formula(), order, operation, operation)(order, operation), predType)

  def visitFormula(ctx: AmecosParser.FormulaContext, order: Order, operation: Operation, ths: Operation): Formula =
    visitDisjunction(ctx.disjunction(), order, operation, ths)

  def visitDisjunction(ctx: AmecosParser.DisjunctionContext, order: Order, operation: Operation, ths: Operation): Formula =
    val formulas = ctx.conjunction().asScala.map(c => visitConjunction(c, order, operation, ths)).toList
    (_, _) => formulas.exists(f => f(order, operation))

  def visitConjunction(ctx: AmecosParser.ConjunctionContext, order: Order, operation: Operation, ths: Operation): Formula =
    val formulas = ctx.unary().asScala.map(u => visitUnary(u, order, operation, ths)).toList
    (_, _) => formulas.forall(f => f(order, operation))

  def visitUnary(ctx: AmecosParser.UnaryContext, order: Order, operation: Operation, ths: Operation): Formula =
    if ctx.NOT() != null then (_, _) => !visitUnary(ctx.unary(), order, operation, ths)(order, operation)
    else (_, _) => visitFormulaAtom(ctx.formulaAtom(), order, operation, ths)(order, operation)

  def visitFormulaAtom(ctx: AmecosParser.FormulaAtomContext, order: Order, operation: Operation, ths: Operation): Formula =
    if ctx.TRUE() != null then return (_, _) => true
    if ctx.FALSE() != null then return (_, _) => false
    if ctx.formula() != null then return visitFormula(ctx.formula(), order, operation, ths)
    if ctx.quantifier() != null then return (order, operation) => visitQuantifier(ctx.quantifier(), order, operation, ths)(order, operation)

    val a = visitValueExpr(ctx.valueExpr(0), order, operation, ths)
    val b = visitValueExpr(ctx.valueExpr(1), order, operation, ths)
    
    val comparator = parseComparator(ctx.comparator())
    (_, _) => a.comp(b, comparator)

  def visitQuantifier(ctx: AmecosParser.QuantifierContext, order: Order, operation: Operation, ths: Operation): Formula =
    val set = visitSetExpr(ctx.setExpr(), order, operation, ths)
    var opSet: Set[Operation] = Set()
    set match
      case OperationDataSet(set) =>
        opSet = set
        if ctx.where() != null then opSet = set.filter(o => visitWhere(ctx.where(), order, o, operation)(order, o))
        if ctx.FORALL() != null then (_, _) => opSet.forall(o => visitFormula(ctx.formula(), order, o, operation)(order, o))
        else (_, _) => opSet.exists(o => visitFormula(ctx.formula(), order, o, operation)(order, o))
        
      case other => throw new IllegalArgumentException("Cannot quantify over non-operation set: " + other)

  def visitWhere(ctx: AmecosParser.WhereContext, order: Order, operation: Operation, ths: Operation): Formula = 
    visitFormula(ctx.formula(), order, operation, ths)
    
  private def parseComparator(ctx: AmecosParser.ComparatorContext): ComparatorTypes =
    ctx.getText match {
      case "==" => ComparatorTypes.eq
      case "!=" => ComparatorTypes.neq
      case "<=" => ComparatorTypes.leq
      case ">=" => ComparatorTypes.geq
      case "<" => ComparatorTypes.lt
      case ">" => ComparatorTypes.gt
      case "same object as" => ComparatorTypes.sameObj
      case "same input as" => ComparatorTypes.sameInput
      case "same output as" => ComparatorTypes.sameOutput
      case other => throw new IllegalArgumentException(s"Unknown comparator: $other")
    }

  /** Evaluates a parsed value expression in the context of an operation and order. */
  def visitValueExpr(ctx: AmecosParser.ValueExprContext, order: Order, operation: Operation, ths: Operation): Data = {
    if ctx.valueAtom() != null then return visitValueAtom(ctx.valueAtom(), order, operation, ths)
    if ctx.mathOp() != null then {
      val op = visitMathOp(ctx.mathOp()).op match {
        case MathOps.plus => (a: Int, b: Int) => a + b
        case MathOps.min => (a: Int, b: Int) => a - b
        case MathOps.times => (a: Int, b: Int) => a * b
        case MathOps.div => (a: Int, b: Int) => a / b
      }
      val a = visitValueExpr(ctx.valueExpr(0), order, operation, ths)
      val b = visitValueExpr(ctx.valueExpr(1), order, operation, ths)
      a match {
        case x: IntData => b match {
          case y: IntData => return IntData(op(x.get, y.get))
          case other => throw IllegalArgumentException("Cannot perform math operation on non-int value: " + other)
        }
        case other => throw IllegalArgumentException("Cannot perform math operation on non-int value: " + other)
      }
    }
    if ctx.index() != null then return visitIndex(ctx.index(), order, operation, ths)
    val set = visitSetExpr(ctx.setExpr(), order, operation, ths)
    IntData(set.data.size)
  }

  def visitIndex(ctx: AmecosParser.IndexContext, order: Order, operation: Operation, tht: Operation ): Data =
    val tuple = visitTupleExpr(ctx.tupleExpr(), operation, tht)
    val idx = Integer.parseInt(ctx.NUM().getText)
    tuple.values.lift(idx).getOrElse(
      throw IllegalArgumentException(s"Index out of bounds at ${ctx.getText}")
    )

  def visitTupleExpr(ctx: AmecosParser.TupleExprContext, operation: Operation, ths: Operation): TupleValue =
    visitTupleAtom(ctx.tupleAtom(), operation, ths)

  def visitTupleAtom(ctx: AmecosParser.TupleAtomContext, operation: Operation, ths: Operation): TupleValue =
    val target = if ctx.getText.startsWith("this") then ths else operation
    InputTuple(target.input.toVector)

  /** Evaluates a literal or operation-output value. */
  def visitValueAtom(ctx: AmecosParser.ValueAtomContext, order: Order, operation: Operation, ths: Operation): Data =
    if ctx.NUM() != null then return IntData(Integer.parseInt(ctx.getText))
    if ctx.operationField() != null then {
      val target = if ctx.getText.startsWith("this") then ths else operation
      if ctx.getText.endsWith("name") then return StringData(target.name)
      return target.output
    }
    if ctx.getText == "name" then return StringData(operation.name)
    if ctx.STRING() != null then return StringData(unquote(ctx.getText))
    if ctx.THIS() != null then return OperationData(ths)
    if ctx.THAT() != null then return OperationData(operation)
    operation.output

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
  def visitSetExpr(ctx: AmecosParser.SetExprContext, order: Order, operation: Operation, ths: Operation): DataSet =
    if ctx.setOperator() != null then
      return combineSets(
        visitSetExpr(ctx.setExpr(0), order, operation, ths),
        visitSetExpr(ctx.setExpr(1), order, operation, ths),
        ctx.setOperator().getText
      )
    val op = if ctx.THIS() != null then ths else operation
    visitSetAtom(ctx.setAtom(), order, op)

  private def combineSets(left: DataSet, right: DataSet, operator: String): DataSet =
    operator match
      case "union" => left.union(right)
      case "intersect" => left.intersect(right)
      case "difference" => left.diff(right)
      case _ => throw new IllegalArgumentException(s"Unknown set operator: $operator")

  def visitSetAtom(ctx: AmecosParser.SetAtomContext, order: Order, operation: Operation): DataSet =
    ctx.getText match {
      case "context" => OperationDataSet(order.context(operation))
      case "future" => OperationDataSet(order.future(operation))
      case "all" => OperationDataSet(history.opExes)
      case other => throw new IllegalArgumentException(s"Unknown set expression: $other")
    }

  /** Resolves consistency names from a `check` clause. */
  override def visitConsistencies(ctx: AmecosParser.ConsistenciesContext): Consistencies =
    Consistencies(ctx.NAME().asScala.map(n => Consistency.get_consistency(n.getText)).toSet)

  /** Creates an object from a `new` declaration. */
  override def visitInit(ctx: AmecosParser.InitContext): InitObj =
    val name = ctx.OBJ().getText
    val object_type = ctx.NAME().getText
    val typeArgs =
      if ctx.typeargs() == null then Nil
      else ctx.typeargs().DATATYPE().asScala.toList.map(parseConcreteType)
    val obj = Object.new_object(object_type, name, factories, typeArgs)
    InitObj(name, obj)

  private def parseConcreteType(token: org.antlr.v4.runtime.tree.TerminalNode): DataTypes =
    token.getText match
      case "int" => DataTypes.Int
      case "string" => DataTypes.String
      case "void" => DataTypes.Unit
      case other => throw IllegalArgumentException(s"Unknown concrete type: $other")

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
    val ret = Option(ctx.argValue()).map(v =>
      if v.NUM() != null then IntData(Integer.parseInt(v.getText))
      else StringData(unquote(v.getText))
    )
    val obj = objMap.getOrElse(obj_name, throw new Exception("Object " + obj_name + " not found: " + ctx.getText))
    try
      Opex(obj.new_op(process, op_name, args, ret, interval.start, interval.end))
    catch
      case e: Exception => throw new IllegalArgumentException(e.getMessage + ": " + ctx.getText)

  /** Converts optional operation arguments to the AST representation. */
  override def visitArgs(ctx: AmecosParser.ArgsContext): Args =
    if ctx == null then Args(List())
    else Args(ctx.argValue().asScala.toList.map(v =>
      if v.NUM() != null then IntData(Integer.parseInt(v.getText))
      else StringData(unquote(v.getText))
    ))

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
