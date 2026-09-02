object Main:
  def main(args: Array[String]): Unit =
    val opExes = Set(Write(1), Read(1))
    val ordering = Map((opExes.head, opExes.tail))
    val history = History(opExes, ordering)

    val legality = history.legal()
    if legality then println("History is legal") else println("History is illegal")