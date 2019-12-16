import scala.io.Source

object Day16 extends App {
  val file = Source.fromFile("./input/day16")
  val input = file.mkString.trim
  file.close()

  val basePattern = Array(0, 1, 0, -1)

  def run(in: Array[Int]): Array[Int] = {
    var arr = in.clone()
    val maxPhases = 100
    val next = Array.fill(in.length)(0)
    for (_ <- 1 to maxPhases) {
      //      println(s"phase $ph / $maxPhases")
      for (i <- arr.indices) {
        val reps = i + 1
        val replength = basePattern.length * reps

        val sum = arr.indices.map(j => {
          val v = arr(j)
          val idx = (j + 1) % replength / reps
          val p = basePattern(idx)
          val res = v * p
          res
        }).sum

        val sumMod = Math.abs(sum % 10)
        next(i) = sumMod
      }

      arr = next
    }

    arr
  }

  var inputArray1: Array[Int] = input.map(c => c.toString.toInt).toArray
  var resultArray1 = run(inputArray1)
  val result1: Any = resultArray1.slice(0, 8).map(i => i.toString).mkString
  println(result1) // 61149209


  def run2(in: Array[Int]): Array[Int] = {
    val maxPhases = 100
    val next = in.clone()

    for (_ <- 1 to maxPhases) {
      //      println(s"phase $ph / $maxPhases")
      for (i <- (0 to next.length - 2).reverse) {
        val v = next(i) + next(i + 1)
        next(i) = Math.abs(v) % 10
      }
    }

    next
  }

  val inputArray2: Array[Int] = input.map(c => c.toString.toInt).toArray
  private val inputArrayConcat: Array[Int] = Array.fill(10000)(inputArray2).flatten
  val messageOffset: Int = inputArrayConcat.take(7).map(c => c.toString.toInt).mkString.toInt
  var inputArrayConcattrunc = inputArrayConcat.drop(messageOffset)
  var resultArray2 = run2(inputArrayConcattrunc)
  var result2 = resultArray2.slice(0, 8).map(c => c.toString.toInt).mkString
  println(result2)
}