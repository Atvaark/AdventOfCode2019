import scala.io.Source

object Day22 extends App {
  val file = Source.fromFile("./input/day22")
  val input = file.getLines().toArray
  file.close()


  def parseInput: Array[(String, Int)] = {
    val re1 = """deal into new stack""".r
    val re2 = """cut (-?\d+)""".r
    val re3 = """deal with increment (\d+)""".r

    input.map(line => {
      line match {
        case re1() => ("reverse", 0)
        case re2(n) => ("cut", n.toInt)
        case re3(n) => ("inc", n.toInt)
      }
    })
  }

  val instructions = parseInput

  def shuffle1(count: BigInt, deckInput: Array[Int]): Array[Int] = {
    var deck = deckInput.clone()

    var i = BigInt(0)
    while (i < count) {
      instructions.foreach {
        case ("reverse", _) =>
          deck = deck.reverse
        //      println("rev", deck.map(i => i.toString).mkString(" "))
        case ("cut", i) =>
          if (i >= 0) {
            deck = deck.slice(i, deck.length).concat(deck.slice(0, i))
          } else deck = deck.slice(deck.length + i, deck.length).concat(deck.slice(0, deck.length + i))
        //      println("cur", deck.map(i => i.toString).mkString(" "))
        case ("inc", i) =>
          var newdeck = Array.fill(deck.length)(0)
          var newPos = 0
          for (idx <- deck.indices) {
            newdeck(newPos) = deck(idx)
            newPos = (newPos + i) % deck.length
          }

          deck = newdeck
        //      println("inc", deck.map(i => i.toString).mkString(" "))
      }

      i += 1

      if (i.mod(BigInt(10000)) == 0) {
        println(i)
      }
    }

    deck
  }

  val count1 = 10007
  val deck = shuffle1(1, (0 until count1).toArray)
  val result1 = deck.zipWithIndex.filter(i => i._1 == 2019).head._2
  println("part1", result1)

  def shuffle2(pos: BigInt, m: BigInt, loops: BigInt): BigInt = {
    var a = BigInt(1)
    var b = BigInt(0)

    for (in <- instructions) {
      in match {
        case ("reverse", _) =>
          a = (-a) % m
          b = (-b - 1) % m
        case ("cut", i) =>
          b = (b - i) % m
        case ("inc", i) =>
          a = (a * i) % m
          b = (b * i) % m
      }
    }

    a = a.modInverse(m)
    b = (-b * a) % m

    var c = BigInt(1)
    var d = BigInt(0)
    var e = loops

    while (e > 0) {
      if ((e & 1) == 1) {
        c = (a * c) % m
        d = ((a * d) + b) % m
      }

      e = e >> 1
      b = ((a * b) + b) % m
      a = (a * a) % m
    }

    ((pos * c) + d) % m
  }

  val cardCount = BigInt("119315717514047")
  val shuffleCount = BigInt("101741582076661")
  val result2 = shuffle2(2020, cardCount, shuffleCount)
  println("part2", result2)
}