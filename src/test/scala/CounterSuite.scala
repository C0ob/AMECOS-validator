import org.scalatest.featurespec.AnyFeatureSpec

import java.nio.file.Paths

class CounterSuite extends AnyFeatureSpec:
  private val counterDir = Paths.get("src", "test", "amecos", "types", "Counter")

  private def parse(fileName: String) =
    HistoryParser.parseFile(counterDir.resolve(fileName).toString)

  private def results(app: Ast.App): List[Int] =
    app.history.opExes.toList
      .sortBy(_.interval._1)
      .map(_.output match
        case IntData(value) => value
        case output => fail(s"Counter.Inc returned $output")
      )

  Feature("Counter"):
    Scenario("returns one for the first increment"):
      val app = parse("first.amecos")

      assert(app.history.opExes.size == 1)
      assert(results(app) == List(1))
      assert(app.history.legal(app.order))

    Scenario("counts preceding increments in an ordered run"):
      val app = parse("ordered.amecos")

      assert(results(app) == List(1, 2, 3))
      assert(app.order.numEdges == 2)
      assert(app.history.legal(app.order))

    Scenario("finds a legal order for overlapping increments"):
      val app = parse("concurrent.amecos")

      assert(results(app).toSet == Set(1, 2))
      assert(app.order.numEdges == 0)
      assert(Order.findValidOrder(app.history, app.consistencies).nonEmpty)

    Scenario("rejects an increment with the wrong result"):
      val app = parse("invalid.amecos")

      assert(results(app) == List(2))
      assert(!app.history.legal(app.order))

    Scenario("accepts increments on different objects"):
      val app = parse("diff_obj.amecos")

      assert(results(app) == List(1, 1))
      assert(app.history.legal(app.order))