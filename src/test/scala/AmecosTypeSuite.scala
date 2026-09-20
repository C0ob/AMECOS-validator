import org.scalatest.featurespec.AnyFeatureSpec

import java.nio.file.Paths

/** Shared helpers for suites backed by `.amecos` fixture files. */
trait AmecosTypeSuite:
  self: AnyFeatureSpec =>

  protected def parseFixture(typeName: String, fileName: String): Ast.App =
    HistoryParser.parseFile(
      Paths.get("src", "test", "amecos", "types", typeName, fileName).toString
    )
