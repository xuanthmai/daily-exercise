import scala.annotation.varargs
object NextGreaterEl {
  def nextGreaterElements(nums: Array[Int]): Array[Int] = {
    var resArr: List[Int] = List.fill(nums.length)(0)

    for (idx <- 0 to nums.length-1) {
      var res = Integer.MIN_VALUE
      var secondIdx = idx
      do {
        secondIdx += 1
        if (nums(secondIdx % nums.length) > nums(idx)) res = nums(secondIdx % nums.length)
      } while ((res == Integer.MIN_VALUE) && (secondIdx % nums.length != idx))
      if (res == Integer.MIN_VALUE) res = -1
      resArr = resArr.updated(idx, res)
    }

    resArr.toArray
  }

  def main(args: Array[String]): Unit = {
    // List(5, 6, 3, 6, -1)
    println(nextGreaterElements(Array(1,5,2,3,6)).toList)
    // List(2, 3, 4, -1, 4)
    println(nextGreaterElements(Array(1,2,3,4,3)).toList)
    // List(8,222,222,-1,222,1111111,-1,1)
    println(nextGreaterElements(Array(1,8,-1,-100,-1,222,1111111,-111111)).toList)
  }
}
