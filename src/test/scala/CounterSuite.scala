import org.scalatest.featurespec.AnyFeatureSpec

class CounterSuite extends AnyFeatureSpec with AmecosTypeSuite:

  private def results(app: Ast.App): List[Int] =
    app.history.opExes.toList
      .sortBy(_.interval._1)
      .map(_.output match
        case IntData(value) => value
        case output => fail(s"Counter.Inc returned $output")
      )

  Feature("Counter"):
    Scenario("returns one for the first increment"):
      val app = parseFixture("Counter", "first.amecos")

      assert(app.history.opExes.size == 1)
      assert(results(app) == List(1))
      assert(app.history.legal(app.order))

    Scenario("counts preceding increments in an ordered run"):
      val app = parseFixture("Counter", "ordered.amecos")

      assert(results(app) == List(1, 2, 3))
      assert(app.order.numEdges == 2)
      assert(app.history.legal(app.order))

    Scenario("finds a legal order for overlapping increments"):
      val app = parseFixture("Counter", "concurrent.amecos")

      assert(results(app).toSet == Set(1, 2))
      assert(app.order.numEdges == 0)
      assert(Order.findValidOrder(app.history, app.consistencies).nonEmpty)

    Scenario("rejects an increment with the wrong result"):
      val app = parseFixture("Counter", "invalid.amecos")

      assert(results(app) == List(2))
      assert(!app.history.legal(app.order))

    Scenario("accepts increments on different objects"):
      val app = parseFixture("Counter", "diff_obj.amecos")

      assert(results(app) == List(1, 1))
      assert(app.history.legal(app.order))
