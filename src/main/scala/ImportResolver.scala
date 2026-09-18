import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** Expands quoted textual imports before the AMECOS source is parsed. */
object ImportResolver:
  private val importPattern = """^\s*import\s+"([^"]+)"\s*$""".r
  private val importPrefix = """^\s*import\b.*$""".r

  /** Recursively expands imports in a source file, resolving paths relative to it. */
  def expand(path: Path): String = expand(path.toAbsolutePath.normalize(), Vector.empty)

  private def expand(path: Path, stack: Vector[Path]): String =
    if stack.contains(path) then
      val cycle = (stack.dropWhile(_ != path) :+ path).mkString(" -> ")
      throw new IllegalArgumentException(s"Import cycle detected: $cycle")

    val content =
      try Files.readString(path, StandardCharsets.UTF_8)
      catch
        case e: Exception =>
          throw new IllegalArgumentException(s"Unable to read imported file $path: ${e.getMessage}", e)

    val lines = content.split("\\r?\\n", -1).toVector
    lines.zipWithIndex.map { case (line, index) =>
      val code = line.take(line.indexOf("//") match
        case -1 => line.length
        case position => position
      )

      code match
        case importPattern(importPath) =>
          val imported = path.getParent.resolve(importPath).toAbsolutePath.normalize()
          expand(imported, stack :+ path)
        case importPrefix() =>
          throw new IllegalArgumentException(
            s"Malformed import in $path:${index + 1}; expected import \"PATH\""
          )
        case _ => line
    }.mkString("\n")
