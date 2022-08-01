import scala.io.Source

object ReverseGame {
  // In case want to read test case from a file
  def readTestcase(path: String): List[Array[String]] = {
    val bufferedSource = Source.fromFile(path)
    val inputArr = for { 
      line <- bufferedSource.getLines()
    } yield line.split(" ")
    bufferedSource.close

    inputArr.toList
  }

  def reverseGame(n: Int, k: Int): Int = {
    // Final value at last idx after moving
    val lastIdxValue: Int = (n-1)/2
    println(s"lastIdxValue = $lastIdxValue")

    val (startIdx, startIdxValue) = 
      if ((n % 2 == 0) && (k < lastIdxValue)) (n-1, lastIdxValue)
      else if ((n % 2 != 0) && (k < lastIdxValue)) (n-2, lastIdxValue-1)
      else if ((n % 2 == 0) && (k > lastIdxValue)) (n-2, lastIdxValue+1)
      else if ((n % 2 != 0) && (k > lastIdxValue)) (n-1, lastIdxValue)
      else (n-1, lastIdxValue)
    println(s"startIdx = $startIdx, startIdxValue = $startIdxValue")

    val numOfJump = Math.abs(startIdxValue - k)
    println(s"numOfJump = $numOfJump")

    startIdx - 2*numOfJump
  }

  def main(args: Array[String]): Unit = {
    println(reverseGame(3, 1))
    println(reverseGame(5, 2))
    println(reverseGame(6, 5))
    println(reverseGame(7, 3))
  }
}
