object LCP {
	def longestCommonPrefix(strs: Array[String]): String = {
		var commonPrefix = ""
		for (index <- 0 to strs(0).length-1) {
			var letter = strs(0)(index)
			for (str <- strs.tail if (index < str.length && str(index) != letter) || (index >= str.length)) return commonPrefix
			commonPrefix += letter
		}
		return commonPrefix
	}

  def main(args: Array[String]): Unit = {
    println(longestCommonPrefix(Array("flow", "flower", "flown")))
    println(longestCommonPrefix(Array("excellent", "good", "job")))
    println(longestCommonPrefix(Array("nugget", "null", "need")))
  }
}
