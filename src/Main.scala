/**
 * Class created by: Artem Stolpovski
 * Copyright 2014
 */

object Main {
  /**
   * Write a recursive function which verifies the balancing of parentheses in a string,
   * which we represent as a List[Char] not a String. For example, the function should return
   * true for the following strings:
   * (if (zero? x) max (/ 1 x))
   * I told him (that it’s not (yet) done). (But he wasn’t listening)
   * The function should return false for the following strings:
   * :-)
   * ())(
   * The last example shows that it’s not enough to verify that a string contains
   * the same number of opening and closing parentheses.
   * @param chars
   * @return
   */
  def balance(chars: List[Char]): Boolean = {
    var openCount = 0
    var closeCount = 0

    def process(part: List[Char]): Boolean = {
      if (part.isEmpty) {
        if (openCount == closeCount) true
        else false

      } else {
        val current = part.head

        if (current == '(') openCount = openCount + 1
        else if (current == ')') {
          if (openCount - 1 != closeCount) return false
          closeCount = closeCount + 1
          if (closeCount > openCount) false
        }
        process(part.tail)
      }
    }

    process(chars)
  }

  /**
   * Write a recursive function that counts how many different ways you can make change
   * for an amount, given a list of coin denominations. For example, there are 3 ways
   * to give change for 4 if you have coins with denomiation 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   *
   * Do this exercise by implementing the countChange function in Main.scala.
   * This function takes an amount to change, and a list of unique denominations
   * for the coins. Its signature is as follows:
   * @param money
   * @param coins
   * @return
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins == null || coins.isEmpty || money == 0) return 0
    var uniqueCoins = coins.distinct.filter(_ <= money).sortWith(_ > _)


    def combinationsWithCoin(money: Int, main: Int, rest: List[Int]): Int = {
      var cmb = 0
      if (main > money) return 0

      for (i <- money / main to 1 by -1) {
        if (money - (i * main) == 0) {
          cmb = cmb + 1
        }
        else if (rest.nonEmpty) {
          cmb = cmb + combinationsWithCoin(money - (main * i), rest.head, rest.tail)
        }
      }
      if (rest.nonEmpty)
        cmb + combinationsWithCoin(money, rest.head, rest.tail)
      else cmb
    }

    val head: Int = uniqueCoins.head
    val tail: List[Int] = uniqueCoins.tail
    if (head == money) {
      uniqueCoins = tail
      1 + combinationsWithCoin(money, tail.head, tail.tail)
    } else {
      combinationsWithCoin(money, head, tail)
    }
  }

  /**
   * The following pattern of numbers is called Pascal’s triangle.
   *     1
   *    1 1
   *   1 2 1
   *  1 3 3 1
   * 1 4 6 4 1
   *    ...
   * The numbers at the edge of the triangle are all 1, and
   * each number inside the triangle is the sum
   * of the two numbers above it. Write a function that computes
   * the elements of Pascal’s triangle by means of a recursive process.
   * @param c column
   * @param r row
   * @return summ for the cell of pascal triangle
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0) 0
    else if (c > r) 0
    else if (c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }


}
