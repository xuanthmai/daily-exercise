object MaxSum {
  def findSecondLenSubarr(arrList: List[List[Int]], secondLen: Int): List[List[Int]] = {
    for {
      arr <- arrList
      idx <- 0 to arr.length-secondLen
    } yield arr.slice(idx, idx + secondLen)
  }

	def maxSumTwoNoOverlap(nums: List[Int], firstLen: Int, secondLen: Int): Int = {
    val allSum = for {
      idx <- 0 to nums.length-firstLen
      firstArr = nums.slice(idx, idx + firstLen)
      secondArr = List(nums.slice(0, idx), nums.slice(idx + firstLen, nums.length))
      subarrSecondArr = findSecondLenSubarr(secondArr, secondLen)
      maxSecondSum = subarrSecondArr.map(_.sum).max
      tempSum = firstArr.sum + maxSecondSum
    } yield tempSum

    allSum.max
 	}       

  def main(args: Array[String]): Unit = {
    // 20
    println(maxSumTwoNoOverlap(List(0,6,5,2,2,5,1,9,4), 1, 2))
    // 29
    println(maxSumTwoNoOverlap(List(3,8,1,3,2,1,8,9,0), 3, 2))
    // 31
    println(maxSumTwoNoOverlap(List(2,1,5,6,0,9,5,0,3,8), 4, 3))

    // memory info
    val mb = 1024*1024
    val runtime = Runtime.getRuntime
    println("ALL RESULTS IN MB")
    println("** Used Memory:  " + (runtime.totalMemory - runtime.freeMemory) / mb)
    println("** Free Memory:  " + runtime.freeMemory / mb)
    println("** Total Memory: " + runtime.totalMemory / mb)
    println("** Max Memory:   " + runtime.maxMemory / mb)
  }
}
