import org.scalatest.featurespec.AnyFeatureSpec

/** Behavioral tests for the consistency conditions defined in the AMECOS PDF. */
class ConsistencySuite extends AnyFeatureSpec with AmecosTypeSuite:

  Feature("Consistency models"):
    Scenario("accept a legal total order"):
      val app = parseFixture("Register", "valid.amecos")

      assert(Atomic.check(app.order))
      assert(Sequential.check(app.order))
      assert(Causal.check(app.order))

    Scenario("causal consistency accepts a partial order but sequential consistency does not"):
      val app = parseFixture("Counter", "concurrent.amecos")
      val order = new Order(app.history)

      assert(Causal.check(order))
      assert(!Sequential.check(order))
      assert(!Atomic.check(order))

    Scenario("sequential consistency does not require real-time order"):
      val app = parseFixture("Counter", "concurrent.amecos")
      val first = app.history.opExes.find(_.process.name == "p1").get
      val second = app.history.opExes.find(_.process.name == "p2").get

      first.interval = (1, 2)
      second.interval = (3, 4)

      val order = new Order(app.history)
      order.add(second, first)

      assert(Sequential.check(order))
      assert(Causal.check(order))
      assert(!Atomic.check(order))
