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
    else pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def countParen(count: Int, chars: List[Char]): Int =
      if (chars.isEmpty) count
      else if (count < 0) count
      else if (chars.head == '(') countParen(count+1, chars.tail)
      else if (chars.head == ')') countParen(count-1, chars.tail)
      else countParen(count, chars.tail)

    if (countParen(0, chars) != 0) false
    else true
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (coins.isEmpty)
      if (money == 0) 1 else 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

  }
