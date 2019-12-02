import scala.io.Source

object Day02 extends App {
  val file = Source.fromFile("./input/day02")
  val lines = file.mkString
  file.close()
  val originalNums: Array[Int] = lines.split(",").map((str: String) => str.trim.toInt)
  PrintNums(originalNums)

  var nums = originalNums.clone()
  nums(1) = 12
  nums(2) = 2
  Run(nums)
  var result = nums(0) // part1
  println(result)

  val target = 19690720
  for (noun <- 0 to 99) {
    for (verb <- 0 to 99) {
      nums = originalNums.clone()
      nums(1) = noun
      nums(2) = verb
      Run(nums)
      val value = nums(0)
      if (value == target) {
        val result2 = 100 * noun + verb
        println(result2) // part2
      }
    }
  }

  private def Run(input: Array[Int]): Unit = {
    var pos = 0
    while (pos < input.length) {
      val op = nums(pos)
      op match {
        case 1 => // Add
          val p1 = nums(pos + 1)
          val p2 = nums(pos + 2)
          val p3 = nums(pos + 3)
          nums(p3) = nums(p1) + nums(p2)

        case 2 => // Mul
          val p1 = nums(pos + 1)
          val p2 = nums(pos + 2)
          val p3 = nums(pos + 3)
          nums(p3) = nums(p1) * nums(p2)

        case 99 =>
          return

        case _ =>
          pos = nums.length
          return
      }

      pos += 4
    }
  }

  private def PrintNums(n: Array[Int]): Unit = {
    n.foreach((i: Int) => {
      print(i)
      print(",")
    })
    println()
  }
}