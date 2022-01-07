package recfun

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    // println("Pascal's Triangle")
    // for row <- 0 to 10 do
    //   for col <- 0 to row do
    //     print(s"${pascal(col, row)} ")
    //   println()
    
    println("balance")
    // // val chars = List('(',')',')','(');
    // // println(chars)
    // val chars1 = "())(".toList
    // println(chars1)
    // // print(s"${balance(col, row)} ")
    // println()
    balanceHelper("())(".toList, 0, 0)

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if r == 0 || c == 0 || c == r then 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
    balanceHelper(chars, 0, 0)

  def balanceHelper(chars: List[Char], leftCnt: Int, rightCnt: Int): Boolean =
    if chars.isEmpty then return leftCnt == rightCnt
    if rightCnt > leftCnt then return false

    if chars.head == '(' then balanceHelper(chars.tail, 1 + leftCnt, rightCnt)
    else if chars.head == ')' then balanceHelper(chars.tail, leftCnt, rightCnt + 1)
    else balanceHelper(chars.tail, leftCnt, rightCnt)

    // if chars.isEmpty then leftCnt == rightCnt
    // else if rightCnt > leftCnt then false
    // else if chars.head == '(' then balanceHelper(chars.tail, 1 + leftCnt, rightCnt)
    // else if chars.head == ')' then balanceHelper(chars.tail, leftCnt, rightCnt + 1)
    // else balanceHelper(chars.tail, leftCnt, rightCnt)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
