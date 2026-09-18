import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, ForkJoinPool, RecursiveAction, RejectedExecutionException}

/** Searches for legal partial orders on a given history using fork-join parallelism. */
object OrderSearch:
  /** Searches for an order satisfying legality and all requested consistency models. */
  def findValidOrder(history: History, consistencies: Set[Consistency]): Option[Order] =
    new SearchRuntime(history, consistencies).run()

  /** Owns the mutable state shared by one parallel search. */
  private final class SearchRuntime(history: History, consistencies: Set[Consistency]):
    private val operations = history.opExes.toVector
    private val pairs =
      for
        left <- operations.indices
        right <- (left + 1) until operations.size
      yield (operations(left), operations(right))

    private val parallelDepth = 2 // Sequential cutoff
    private val pool = new ForkJoinPool()
    private val result = new CompletableFuture[Option[Order]]()
    private val outstandingTasks = new AtomicInteger(0)

    def run(): Option[Order] =
      try
        submit(new SearchTask(0, Order(history), 0))
        result.join()
      finally
        pool.shutdownNow()

    private def submit(task: SearchTask): Unit =
      if !result.isDone then
        outstandingTasks.incrementAndGet()
        try pool.execute(task)
        catch
          case _: RejectedExecutionException => outstandingTasks.decrementAndGet()

    private def completeResult(order: Order): Unit =
      result.complete(Some(order))

    private def valid(order: Order): Boolean =
      history.legal(order) && consistencies.forall(_.check(order))

    private def searchSequential(index: Int, order: Order): Option[Order] =
      if result.isDone then None
      else if valid(order) then Some(order)
      else if index == pairs.size then None
      else
        val (left, right) = pairs(index)
        val withoutEdge = searchSequential(index + 1, order)
        if withoutEdge.isDefined then withoutEdge
        else
          val leftFirstResult = withEdge(order, left, right)
            .flatMap(candidate => searchSequential(index + 1, candidate))
          if leftFirstResult.isDefined then leftFirstResult
          else
            withEdge(order, right, left)
              .flatMap(candidate => searchSequential(index + 1, candidate))

    private def branches(index: Int, order: Order): List[Order] =
      val (left, right) = pairs(index)
      List(
        Some(copyOrder(order)),
        withEdge(order, left, right),
        withEdge(order, right, left)
      ).flatten

    private def copyOrder(order: Order): Order =
      val copy = Order(history)
      operations.foreach { before =>
        order.next(before).foreach(after => copy.add(before, after))
      }
      copy

    private def withEdge(order: Order, before: Operation, after: Operation): Option[Order] =
      if order.future(after).contains(before) then None
      else
        val candidate = copyOrder(order)
        candidate.add(before, after)
        Some(candidate)

    private final class SearchTask(index: Int, order: Order, depth: Int)
      extends RecursiveAction:
      override def compute(): Unit =
        try
          if !result.isDone then
            if valid(order) then completeResult(order)
            else if index == pairs.size then ()
            else if depth >= parallelDepth then
              searchSequential(index, order).foreach(completeResult)
            else
              branches(index, order).foreach { branch =>
                submit(new SearchTask(index + 1, branch, depth + 1))
              }
        catch
          case error: Throwable =>
            if !result.isDone then result.completeExceptionally(error)
        finally
          if outstandingTasks.decrementAndGet() == 0 then
            result.complete(None)
