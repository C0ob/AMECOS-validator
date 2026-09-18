import scala.collection.mutable

/** A partial order over the operations in a history. */
class Order(val history: History):

  /** Indexes in this array identify operations in the adjacency sets. */
  private val operations: Vector[Operation] = history.opExes.toVector
  private val opMap: Map[Operation, Int] = operations.zipWithIndex.toMap
  /** Direct successors and predecessors. */
  private val adjacency: Array[mutable.Set[Int]] =
    Array.fill(operations.length)(mutable.Set.empty[Int])
  private val reverseAdjacency: Array[mutable.Set[Int]] =
    Array.fill(operations.length)(mutable.Set.empty[Int])
  /** Reachability caches, invalidated whenever a new edge is added. */
  private val futureMemo: Array[Option[Set[Int]]] =
    Array.fill(operations.length)(None)
  private val contextMemo: Array[Option[Set[Int]]] =
    Array.fill(operations.length)(None)
  /** Number of direct edges added to this order. */
  var numEdges = 0

  /** Adds a direct ordering edge: `before` must precede `after`. */
  def add(before: Operation, after: Operation): Unit =
    val beforeId = idOf(before)
    val afterId = idOf(after)
    if beforeId == afterId || reachableIds(afterId, adjacency, futureMemo).contains(beforeId) then
      throw new IllegalArgumentException("Ordering must be acyclic")
    numEdges += 1

    if adjacency(beforeId).add(afterId) then
      reverseAdjacency(afterId).add(beforeId)
      resetMemo()

  /** Adds several direct ordering edges. */
  def addAll(edges: Iterable[(Operation, Operation)]): Unit =
    edges.foreach { case (before, after) => add(before, after) }

  /** Direct successors of an operation. */
  def next(op: Operation): Set[Operation] =
    adjacency(idOf(op)).iterator.map(operations).toSet

  private def idOf(op: Operation): Int =
    opMap.getOrElse(op, throw new IllegalArgumentException("Operation is not part of this order"))

  /** Direct predecessors of an operation. */
  def prev(op: Operation): Set[Operation] =
    reverseAdjacency(idOf(op)).iterator.map(operations).toSet

  /** All operations transitively ordered before `op`. */
  def context(op: Operation): Set[Operation] =
    reachableIds(idOf(op), reverseAdjacency, contextMemo).iterator.map(operations).toSet

  /** All operations transitively ordered after `op`. */
  def future(op: Operation): Set[Operation] =
    reachableIds(idOf(op), adjacency, futureMemo).iterator.map(operations).toSet

  /** Prints the direct-edge graph in a terminal-friendly format. */
  def print(): Unit =
    println(s"+-- Order graph (${operations.size} operations)")
    if operations.isEmpty then
      println("|  (empty)")
    else
      operations.indices.foreach { id =>
        val operation = operations(id)
        println(f"|  [$id%02d] $operation")

        val predecessors = reverseAdjacency(id).toVector.sorted
        if predecessors.nonEmpty then
          println(s"|       <- ${predecessors.map(reference).mkString(", ")}")

        val successors = adjacency(id).toVector.sorted
        if successors.nonEmpty then
          println(s"|       -> ${successors.map(reference).mkString(", ")}")
      }
    println("+-- End order graph")

  private def reference(id: Int): String =
    f"[$id%02d]"

  private def reachableIds(
                            start: Int,
                            edges: Array[mutable.Set[Int]],
                            memo: Array[Option[Set[Int]]]
                          ): Set[Int] =
    memo(start) match
      case Some(reachable) => reachable
      case None =>
        val reachable = mutable.Set.empty[Int]
        edges(start).foreach { nextId =>
          reachable += nextId
          reachable ++= reachableIds(nextId, edges, memo)
        }
        val result = reachable.toSet
        memo(start) = Some(result)
        result

  private def resetMemo(): Unit =
    futureMemo.indices.foreach { id =>
      futureMemo(id) = None
      contextMemo(id) = None
    }


object Order:
  /** Finds an order that is legal and satisfies every requested consistency. */
  def findValidOrder(history: History, consistencies: Set[Consistency]): Option[Order] =
    OrderSearch.findValidOrder(history, consistencies)
