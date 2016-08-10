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
  def getNthPascalRow(n: Int, row: List[Int]): List[Int] = {
    if (n == 0) row
    else {
      var newRow = List(1)
      if (row.length > 1) {
        for (i <- 0 until row.length - 1) {
          val newCell = row(i) + row(i + 1)
          newRow = newRow ::: List(newCell)
        }
      }
      getNthPascalRow(n-1, newRow ::: List(1))
    }
  }

    def pascal(c: Int, r: Int): Int = {
      val row = getNthPascalRow(r, List(1))
      row(c)
    }
  
  /**
   * Exercise 2
   */
  def trimFront(chars: List[Char]): List[Char] = {
    if (chars.isEmpty) chars
    else {
      if (chars.head == '(' || chars.head == ')') chars
      else trimFront(chars.tail)
    }
  }

  def isBalanced(chars: List[Char], opened: Int): Boolean = {
    val trimmed = trimFront(chars)
    if (opened < 0) false
    else if (trimmed.isEmpty) (opened == 0)
    else{
      if (trimmed.head == '(') isBalanced(trimmed.tail, opened+1)
      else isBalanced(trimmed.tail, opened-1)
    }
  }

  def balance(chars: List[Char]): Boolean = {
    isBalanced(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = _countChange(money, coins.sorted)

  def _countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else{
      var count = 0
      for ((coin, i) <- coins.zipWithIndex) {
        count += _countChange(money - coin, coins.drop(i))
      }
      count
    }
  }
  }
