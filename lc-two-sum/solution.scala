object TwoSum {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    for (curInd <- 0 to nums.length-1) {
      for (sumInd <- curInd+1 to nums.length-1) {
        if (nums(curInd) + nums(sumInd) == target) return Array(curInd, sumInd)
      }
    }
       
    return Array(0, 0)
  }

  def main(args: Array[String]): Unit = {
    println(twoSum(Array(3,1,6,9,5), 7).toList)
    println(twoSum(Array(12,53,2,0,2,9,8), 10).toList)
    println(twoSum(Array(0,0,0,3,10,2,9), 9).toList)
  }
}
