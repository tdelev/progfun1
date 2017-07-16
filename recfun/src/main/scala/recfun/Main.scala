package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    //println(pascal(1, 2))
    //println(balance("(if (zero? x) max (/ 1 x))".toList))
    //println(balance(":)".toList))
    //println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    //println(balance("())(".toList))
    //println(balance("(()((())))".toList))
    /*println(countChange(4, List(1, 2)))
    println(countChange(5, List(2, 3)))
    println(countChange(3, List(2, 3)))
    println(countChange(5, List(1, 2, 3)))*/
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r == 0 && c == 0) 1
    else if (c < 0 || r < 0) 0
    else if (c == 1 || (c + 1) == r) r
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def count(n: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) n
      else if (n < 0) n
      else if (chars.head == '(')
        count(n + 1, chars.tail)
      else if (chars.head == ')')
        count(n - 1, chars.tail)
      else
        count(n, chars.tail)
    }
    count(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def count(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money <= 0) 0
      else if (money - coins.head == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

    count(money, coins.sorted)
  }
}
