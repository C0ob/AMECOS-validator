import org.scalatest.featurespec.AnyFeatureSpec

/** Behavioral tests for the consistency conditions defined in the AMECOS PDF. */
class ConsistencySuite extends AnyFeatureSpec with AmecosTypeSuite:

  Feature("Consistency models"):
    Scenario("accept a legal total order"):
      val app = parseFixture("Register", "valid.amecos")

      assert(Linearizability.check(app.order))
      assert(SeqCons.check(app.order))
      assert(CausalCons.check(app.order))

    Scenario("causal consistency accepts a partial order but sequential consistency does not"):
      val app = parseFixture("Counter", "concurrent.amecos")
      val order = new Order(app.history)

      assert(CausalCons.check(order))
      assert(!SeqCons.check(order))
      assert(!Linearizability.check(order))

    Scenario("sequential consistency does not require real-time order"):
      val app = parseFixture("Counter", "concurrent.amecos")
      val first = app.history.opExes.find(_.process.name == "p1").get
      val second = app.history.opExes.find(_.process.name == "p2").get

      first.interval = (1, 2)
      second.interval = (3, 4)

      val order = new Order(app.history)
      order.add(second, first)

      assert(SeqCons.check(order))
      assert(CausalCons.check(order))
      assert(!Linearizability.check(order))
