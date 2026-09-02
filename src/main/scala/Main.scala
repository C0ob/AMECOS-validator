object Main:
  def main(args: Array[String]): Unit =
    val op1 = Write(1)
    val op2 = Read(1)
    val op3 = Write(2)
    val op4 = Read(2)
    val opExes = Set(op1, op2, op3, op4)
    val ordering = Map((op1, Set(op2)), (op2, Set(op3)), (op3, Set(op4)))
    val history = History(opExes, ordering)

    val legality = history.legal()
    if legality then println(Console.GREEN + "History is legal") else println(Console.RED + "History is illegal")