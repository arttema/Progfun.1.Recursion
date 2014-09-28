/**
 * Healthland © scala1 2014
 * All rights reserved
 * Created by Artem Stolpovski on 22.09.2014.
 */
object Main {


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
          if(openCount -1 != closeCount) return false
          closeCount = closeCount + 1
          if (closeCount > openCount) false
        }
        process(part.tail)
      }
    }

    process(chars)
  }

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
