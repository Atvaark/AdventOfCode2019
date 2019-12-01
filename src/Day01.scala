import scala.io.Source

object Day01 extends App {
  val lines = Source.fromFile("./input/day01").getLines.toArray
  var sum1 = 0
  var sum2 = 0
  var getfuel = (i: Int) => {
    val f = (i / 3) - 2
    if (f >= 0) f else 0
  }
  lines.foreach(line => {
    var fuel = getfuel(line.toInt)
    sum1 += fuel
    sum2 += fuel

    while (fuel > 0) {
      fuel = getfuel(fuel)
      sum2 += fuel
    }
  })
  println(sum1) // part1
  println(sum2) // part2
}