object MaxSum {
  def findSecondLenSubarr(arrList: List[List[Int]], secondLen: Int): List[List[Int]] = {
    // arrList.foreach(arr => {
      // for (idx <- 0 to arr.length-secondLen) yield arr.slice(idx, idx + secondLen)
    // })

    for {
      arr <- arrList
      idx <- 0 to arr.length-secondLen
    } yield arr.slice(idx, idx + secondLen)
  }

  def maxSumInArr(arrList: List[List[Int]]): Int = {
    arrList.map(_.sum).max
  }

	def maxSumTwoNoOverlap(nums: List[Int], firstLen: Int, secondLen: Int): Int = {
    var maxSum: Int = 0

    for {
      idx <- 0 to nums.length-firstLen
      firstArr = nums.slice(idx, idx + firstLen)
      secondArr = List(nums.slice(0, idx), nums.slice(idx + firstLen, nums.length))
      subarrSecondArr = findSecondLenSubarr(secondArr, secondLen)
      maxSecondSum = subarrSecondArr.map(_.sum).max
      tempSum = firstArr.sum + maxSecondSum
      maxSum = tempSum if maxSum > tempSum
    } yield maxSum
 	}       

  def main(args: List[String]): Unit = {
    maxSumTwoNoOverlap(List(0,6,5,2,2,5,1,9,4), 1, 2)
  }
}
