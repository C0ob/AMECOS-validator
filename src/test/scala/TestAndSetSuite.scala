import org.scalatest.featurespec.AnyFeatureSpec

class TestAndSetSuite extends AnyFeatureSpec with AmecosTypeSuite:

  private def results(app: Ast.App): List[Int] =
    app.history.opExes.toList
      .sortBy(_.interval._1)
      .map(_.output match
        case IntData(value) => value
        case output => fail(s"TestAndSet.testandset returned $output")
      )

  Feature("Test and Set"):

    Scenario("returns zero before the flag is set and one afterwards"):
      val app = parseFixture("TestAndSet", "valid.amecos")

      assert(results(app) == List(0, 1, 1))
      assert(app.order.numEdges == 2)
      assert(app.history.legal(app.order))

    Scenario("rejects a result outside the test-and-set range"):
      val app = parseFixture("TestAndSet", "two.amecos")

      assert(results(app) == List(0, 1, 2))
      assert(!app.history.legal(app.order))

    Scenario("rejects a test-and-set result with an invalid order"):
      val app = parseFixture("TestAndSet", "invalid.amecos")

      assert(results(app) == List(0, 1, 1))
      assert(!app.history.legal(app.order))
