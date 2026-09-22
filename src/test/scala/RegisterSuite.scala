import org.scalatest.featurespec.AnyFeatureSpec

class RegisterSuite extends AnyFeatureSpec with AmecosTypeSuite:
  private def readResult(app: Ast.App): Int =
    app.history.opExes.find(_.name == "Read").get.output match
      case IntData(value) => value
      case output => fail(s"Register.Read returned $output")

  Feature("Register"):
    Scenario("reads the value of a preceding write"):
      val app = parseFixture("Register", "valid.amecos")

      assert(readResult(app) == 7)
      assert(app.history.legal(app.order))

    Scenario("requires a preceding write to the same register"):
      val app = parseFixture("Register", "missing_write.amecos")

      assert(!app.history.legal(app.order))

    Scenario("rejects a stale value after a later write"):
      val app = parseFixture("Register", "stale_read.amecos")

      assert(readResult(app) == 7)
      assert(!app.history.legal(app.order))

    Scenario("keeps register objects independent"):
      val app = parseFixture("Register", "different_object.amecos")

      assert(!app.history.legal(app.order))

    Scenario("validates write and read arguments"):
      intercept[IllegalArgumentException](parseFixture("Register", "invalid_write.amecos"))
      intercept[IllegalArgumentException](parseFixture("Register", "invalid_read.amecos"))

    Scenario("specializes the generic register with different types"):
      val app = parseFixture("Register", "generic_types.amecos")

      val results = app.history.opExes.filter(_.name == "Read").map(_.output)
      assert(results.contains(IntData(7)))
      assert(results.contains(StringData("seven")))
      assert(app.history.legal(app.order))
