package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def add_char_balance(ch: Char): Int = {
      if (ch.equals('(')) 1
      else if (ch.equals(')')) -1
      else 0
    }

    def chars_loop(L: List[Char], balance: Int): Boolean = {
      if (!L.isEmpty) {
        if (balance != -1)
          chars_loop(L.tail, balance + add_char_balance(L.head))
        else false
      }
      else {
        if (balance != 0) false
        else true
      }
    }

    chars_loop(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      1
    else if (money > 0 && !coins.isEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
      0
  }
}
