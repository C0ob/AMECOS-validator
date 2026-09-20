import org.scalatest.featurespec.AnyFeatureSpec

class DictionarySuite extends AnyFeatureSpec with AmecosTypeSuite:
  private def getResult(app: Ast.App): String =
    app.history.opExes.find(_.name == "Get").get.output match
      case StringData(value) => value
      case output => fail(s"Dictionary.Get returned $output")

  Feature("Dictionary"):
    Scenario("gets the value of a preceding put"):
      val app = parseFixture("Dictionary", "valid.amecos")

      assert(getResult(app) == "one")
      assert(app.history.legal(app.order))

    Scenario("requires a preceding put for a key"):
      val app = parseFixture("Dictionary", "missing_put.amecos")

      assert(!app.history.legal(app.order))

    Scenario("rejects a value that does not match the put"):
      val app = parseFixture("Dictionary", "wrong_value.amecos")

      assert(getResult(app) == "two")
      assert(!app.history.legal(app.order))

    Scenario("rejects a stale value after a later put"):
      val app = parseFixture("Dictionary", "stale_get.amecos")

      assert(getResult(app) == "one")
      assert(!app.history.legal(app.order))

    Scenario("keeps dictionary keys independent"):
      val app = parseFixture("Dictionary", "different_key.amecos")

      assert(getResult(app) == "one")
      assert(app.history.legal(app.order))

    Scenario("accepts read of overwritten value"):
      val app = parseFixture("Dictionary", "overwritten.amecos")

      assert(getResult(app) == "two")
      assert(app.history.legal(app.order))
