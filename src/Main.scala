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
          closeCount = closeCount + 1
          if (closeCount > openCount) false
        }
        process(part.tail)
      }
    }

    process(chars)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins == null) return 0
    if (coins.isEmpty) return 0
    if (money == 0) return 0
    val uniqueCoins = coins.distinct
    var combination = 0

    def checkDiv(main: Int, rest: List[Int]) {
      if (rest == null) return
      if (rest.isEmpty) return
      if (money == 0) return

      if(main==0 && rest.size > 1 && rest(1) != 0) checkDiv(rest(1), rest.tail)

      val mod: Int = money % main
      if (mod == 0) combination = combination + 1

      for (coin <- rest) {
        if (mod!= 0 && coin % mod == 0) combination = combination + 1
        else if (rest(1) != 0) checkDiv(rest(1), rest.tail)
      }
    }

    uniqueCoins.foreach(checkDiv(_, uniqueCoins.tail))
    combination
  }

  /**
   *
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
