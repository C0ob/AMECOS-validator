import Ast.App

import java.nio.file.{Path, Paths}

/** Command-line entrypoint for validating and visualizing one `.amecos` file. */
object Main:
  private enum DiagramFormat(val extension: String):
    case Svg extends DiagramFormat("svg")
    case Png extends DiagramFormat("png")
    case Pdf extends DiagramFormat("pdf")

  private case class Options(path: String, diagram: Boolean, format: DiagramFormat)

  private def parseOptions(args: Array[String]): Either[String, Options] =
    var diagram = false
    var format = DiagramFormat.Svg
    var path: Option[String] = None
    var index = 0

    while index < args.length do
      args(index) match
        case "--diagram" => diagram = true
        case "--format" =>
          index += 1
          if index >= args.length then return Left("--format requires svg, png, or pdf")
          args(index).toLowerCase match
            case "svg" => format = DiagramFormat.Svg
            case "png" => format = DiagramFormat.Png
            case "pdf" => format = DiagramFormat.Pdf
            case value => return Left(s"Unknown diagram format: $value")
        case value if value.startsWith("--") => return Left(s"Unknown option: $value")
        case value =>
          if path.isDefined then return Left("Please provide exactly one filepath")
          path = Some(value)
      index += 1

    path match
      case Some(value) => Right(Options(value, diagram, format))
      case None => Left("Please provide filepath")

  private def writeDiagram(inputPath: String, history: History, format: DiagramFormat): Unit =
    val input = Paths.get(inputPath)
    val base = removeExtension(input)
    val imagePath = Paths.get(base.toString + "." + format.extension)
    val svg = HistoryDiagram.render(history)
    HistoryDiagram.renderImage(svg, imagePath, format.extension)
    println(s"Diagram image written to $imagePath")

  private def removeExtension(path: Path): Path =
    val fileName = path.getFileName.toString
    val dot = fileName.lastIndexOf('.')
    if dot <= 0 then path.resolveSibling(fileName)
    else path.resolveSibling(fileName.substring(0, dot))

  /** Validates the supplied file and optionally writes its process diagram. */
  def main(args: Array[String]): Unit =
    parseOptions(args) match
      case Left(error) =>
        println(Console.RED + error + Console.RESET)
      case Right(options) =>
        var ast: App = null
        try ast = HistoryParser.parseFile(options.path)
        catch
          case e: Exception =>
            println(Console.RED + "Error parsing file: " + e.getMessage + Console.RESET)
            return

        val history = History(ast.opexes.toSet, ast.ordering)
        val legality = history.legal()
        if legality then println(Console.GREEN + "History is legal" + Console.RESET)
        else println(Console.RED + "History is illegal" + Console.RESET)

        if ast.consistensies.nonEmpty then println("!--- Checking consistencies:")
        ast.consistensies.foreach(c =>
          if c.check(history) then println(Console.GREEN + c.name + " is satisfied" + Console.RESET)
          else println(Console.RED + c.name + " is not satisfied" + Console.RESET)
        )

        if options.diagram then
          try writeDiagram(options.path, history, options.format)
          catch
            case e: Exception =>
              println(Console.RED + "Error generating diagram: " + e.getMessage + Console.RESET)
