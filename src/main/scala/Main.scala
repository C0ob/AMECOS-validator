import Amecos.{AmecosLexer, AmecosParser}
import Ast.App
import org.antlr.v4.runtime.*

import java.nio.file.Paths

object Main:
  private def parseFile(path: String): App =
    val input = CharStreams.fromPath(Paths.get(path))
    val lexer = new AmecosLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new AmecosParser(tokens)

    parser.removeErrorListeners()
    parser.addErrorListener(new BaseErrorListener {
      override def syntaxError(
                                recognizer: Recognizer[_, _],
                                offendingSymbol: Any,
                                line: Int,
                                charPositionInLine: Int,
                                msg: String,
                                e: RecognitionException
                              ): Unit = {
        throw new RuntimeException(s"Syntax error at $line:$charPositionInLine — $msg")
      }
    })

    val tree = parser.app()
    new AstBuilder().visitApp(tree)

  def main(args: Array[String]): Unit =
    if args.isEmpty then {
      println(Console.RED + "Please provide filepath")
      return
    }

    var ast: App = null
    try {
      ast = parseFile(args(0))
    } catch {
      case e: Exception =>
        println(Console.RED + "Error parsing file: " + e.getMessage + Console.RESET)
        return
    }
    val history = History(ast.opexes.toSet, ast.ordering)

    val legality = history.legal()
    if legality then println(Console.GREEN + "History is legal" + Console.RESET) else println(Console.RED + "History is illegal" + Console.RESET)

    if ast.consistensies.nonEmpty then println("!--- Checking consistencies:")
    ast.consistensies.foreach(c =>
      if c.check(history) then
        println(Console.GREEN + c.name + " is satisfied" + Console.RESET)
        else
        println(Console.RED + c.name + " is not satisfied" + Console.RESET)
    )