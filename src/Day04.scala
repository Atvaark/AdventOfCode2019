import scala.io.Source

object Day04 extends App {
  val file = Source.fromFile("./input/day04")
  val line = file.mkString.trim
  file.close()

  val ranges: Array[Int] = line.split('-').map((str: String) => str.toInt)
  val min = ranges(0)
  val max = ranges(1)


  var count1 = 0
  var count2 = 0
  //  val len = 4
  for (n <- min to max) {
    var nstring = n.toString //n.toString.reverse.padTo(len, '0').reverse

    var adjacentFound1 = false
    for (i <- 0 to nstring.length - 2) {
      if (!adjacentFound1 && nstring(i) == nstring(i + 1)) {
        adjacentFound1 = true
      }
    }

    var rep = 0
    var prev = ' '
    var twoFound = false
    for (i <- 0 to nstring.length) {
      if (i < nstring.length) {
        val c = nstring(i)
        if (c == prev) {
          rep += 1
        } else {
          twoFound = twoFound || rep == 2
          rep = 1
        }

        prev = c
      } else {
        twoFound = twoFound || rep == 2
      }
    }

    if (adjacentFound1 || twoFound) {
      var ordered = true
      var pc = nstring.head
      nstring.tail.foreach((c: Char) => {
        if (ordered && pc > c) {
          ordered = false
        } else {
          pc = c
        }
      })

      if (ordered) {
        if (adjacentFound1) {
          count1 += 1
        }

        if (twoFound) {
          count2 += 1
        }
      }
    }
  }
  println("result1", count1)
  println("result2", count2)
}