object MinDel {
  var logger: Boolean = false

  def log(msg: String): Unit = {
    if (logger) println(msg)
  }

  // @TODO refactor for cleaner code pretty please
  def minDeletions(s: String): Int = {
    val charFreq: Array[(Char, Int)] = s.distinct.toCharArray.map(ch => (ch, s.count(_ == ch)))
    log(s"charFreq = ${charFreq.toList}")
    var frequencyList: Array[Int] = charFreq.map(_._2)
    log(s"original frequencyList = ${frequencyList.toList}")
    var deletion: Int = 0
    frequencyList.zipWithIndex.foreach(freqWithIdx => {
      var newFreq = freqWithIdx._1
      while (frequencyList.filter(_ == newFreq).length > 1) {
        newFreq -= 1
        deletion += 1
        log(s"newFreq = $newFreq")
        
        frequencyList = 
          if (newFreq == 0) frequencyList.updated(freqWithIdx._2, -1)
          else frequencyList.updated(freqWithIdx._2, newFreq)

        log(s"updated frequencyList = ${frequencyList.toList}")
      }
    })

    deletion
  }

  def main(args: Array[String]): Unit = {
    // some working cases
    // 4
    println(minDeletions("ababccd"))
    // 2
    println(minDeletions("ceabaacb"))
    // 2
    println(minDeletions("aaabbbcc"))
    // 0
    println(minDeletions("aab"))
    // 3
    println(minDeletions("abcd"))

    // scala MinDel astring false
    logger = args(1).toBoolean
    println(minDeletions(args(0)))
  }
}
