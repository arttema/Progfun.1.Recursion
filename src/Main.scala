/**
 * Healthland © scala1 2014
 * All rights reserved
 * Created by Artem Stolpovski on 22.09.2014.
 */
object Main {
  def main(args: Array[String]): Unit ={

    print(balance("Just na(asds) assda".toList))
    print(balance("=))".toList))

    print(balance("asddas(asd)((asddsa)".toList))
    print(balance("())(".toList))
  }

  def balance(chars: List[Char]): Boolean = {
    var openCount = 0
    var closeCount = 0
    var break = false

    def process(part: List[Char]): Boolean = {
      if(part.isEmpty){
        if(openCount == closeCount)true
        else false
      }else{
        val current = part.head
        if (current == '(')openCount = openCount+1
        else if (current == ')'){
          closeCount = closeCount + 1
          if(closeCount > openCount)return false
        }
        process(part.tail)
      }
    }

    process(chars)
  }


}
