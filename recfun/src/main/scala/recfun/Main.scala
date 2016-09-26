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
      def outOfBounds(c: Int, r: Int): Boolean = {
        val negative  = c < 0 || r < 0
        val pastMaxColumn = c > r
        negative || pastMaxColumn
      }
      val bothTopWithinBounds = !outOfBounds(c-1, r-1) && !outOfBounds(c, r-1)

      if (bothTopWithinBounds) {
        val upperLeft = pascal(c-1, r-1)
        val upperRight = pascal(c, r-1)
        upperLeft + upperRight
      } else {
        1
      }
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def isParenBalanced(chars: List[Char], numberofOpen: Int): Boolean = {
        if (chars.isEmpty) {
          numberofOpen == 0
        } else if (chars.head == '(') {
          isParenBalanced(chars.tail, numberofOpen+1)
        } else if (chars.head == ')') {
          numberofOpen > 0 && isParenBalanced(chars.tail, numberofOpen-1)
        } else {
          isParenBalanced(chars.tail, numberofOpen)
        }

      }
      isParenBalanced(chars, 0)
    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if ((money < 0) || coins.isEmpty) {
        0
      }
      else {
        if (money == 0) {
          1
        }
        else {
          countChange(money - coins.head, coins) + countChange(money, coins.tail)
        }
      }
    }

  }
