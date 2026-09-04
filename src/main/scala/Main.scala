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
    if args.isEmpty then throw IllegalArgumentException("Please provide filepath")

    val ast: App = parseFile(args(0))
    val history = History(ast.opexes.toSet, ast.ordering)

    val legality = history.legal()
    if legality then println(Console.GREEN + "History is legal") else println(Console.RED + "History is illegal")