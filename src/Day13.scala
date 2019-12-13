import scala.collection.mutable
import scala.io.Source

object Day13 extends App {
  val file = Source.fromFile("./input/day13")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Long] = lines.split(",").map((str: String) => str.trim.toLong)

  val map = createMap()
  val score = play(map)
  println("score", score)

  def createMap(): mutable.Map[(Long, Long), Long] = {
    val indexedInstructions: Array[(Long, Long)] = originalInstructions.zipWithIndex.map((tuple: (Long, Int)) => (tuple._2.toLong, tuple._1))
    val mappedInstructions: mutable.Map[Long, Long] = mutable.Map.from(indexedInstructions)
    val software = new ControlSoftware(0, mappedInstructions, Array.empty)

    val map = mutable.Map[(Long, Long), Long]() // /pos -> id
    while (!software.isHalted) {
      val x1 = software.run()
      val y1 = software.run()
      val id1 = software.run()

      (x1, y1, id1) match {
        case (Some(x), Some(y), Some(id)) =>
          map((x, y)) = id

        case _ =>

      }
    }

    val count = map.values.count(id => id == 2)
    println("count", count)

    //    printMap(map)
    map
  }

  def printMap(map: mutable.Map[(Long, Long), Long]): Unit = {
    val minX = map.keys.minBy(pos => pos._1)._1
    val maxX = map.keys.maxBy(pos => pos._1)._1
    val minY = map.keys.minBy(pos => pos._2)._2
    val maxY = map.keys.maxBy(pos => pos._2)._2

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        val id = map.getOrElse((x, y), 0)
        val c = id match {
          case 0 => '.'
          case 1 => 'W'
          case 2 => '#'
          case 3 => 'P'
          case 4 => 'O'
        }

        print(c)
      }

      println()
    }
    println()
  }

  def play(map: mutable.Map[(Long, Long), Long]): Long = {
    val indexedInstructions: Array[(Long, Long)] = originalInstructions.zipWithIndex.map((tuple: (Long, Int)) => (tuple._2.toLong, tuple._1))
    val mappedInstructions: mutable.Map[Long, Long] = mutable.Map.from(indexedInstructions)
    mappedInstructions(0) = 2
    val software = new ControlSoftware(0, mappedInstructions, Array.empty)

    printMap(map)
    println()

    var score = 0L
    software.input = software.input.appended(0)


    var ball = map.filter(kv => kv._2 == 4).head._1
    var paddle = map.filter(kv => kv._2 == 3).head._1

    def getDir: Long = {
      val dir = if (ball._1 < paddle._1) {
        -1
      } else if (ball._1 > paddle._1) {
        1
      } else {
        0
      }

      dir
    }

    while (!software.isHalted) {
      val x1 = software.run()
      val y1 = software.run()
      val id1 = software.run()

      (x1, y1, id1) match {
        case (Some(x), Some(y), Some(id)) =>
          if (x == -1 && y == 0) {
            score = id
          } else {
            map((x, y)) = id
            id match {
              case 3 =>
                paddle = (x, y)
              case 4 =>
                ball = (x, y)

                software.input = Array(getDir)
              case _ =>
            }
          }
        case _ =>
      }


      printMap(map)
      println()

    }


    printMap(map)
    println()

    score
  }

  //  def printMap(map: Map[(Int, Int), Long]): Unit = {
  //    val minX = map.keys.map(pos => pos._1).min
  //    val maxX = map.keys.map(pos => pos._1).max
  //    val minY = map.keys.map(pos => pos._2).min
  //    val maxY = map.keys.map(pos => pos._2).max
  //
  //    for (y <- minY to maxY) {
  //      for (x <- minX to maxX) {
  //        map.getOrElse((x, y), 0) match {
  //          case 0 =>
  //            print(".")
  //          case 1 =>
  //            print("#")
  //        }
  //      }
  //      println()
  //    }
  //    println()
  //  }

  class ControlSoftware(val id: Long, var instructions: mutable.Map[Long, Long], var input: Array[Long]) {
    private var pos: Long = 0
    private var relativeBase: Long = 0
    var isHalted = false

    def run(): Option[Long] = {
      while (instructions.contains(pos)) {
        val instruction: Long = instructions(pos)
        val op: Long = instruction % 100
        //        println(instruction)

        val modes = if (instruction.toString.length >= 2) {
          instruction.toString.reverse.substring(2).map(c => c.toString.toLong).toArray
        } else {
          Array[Long]()
        }

        op match {
          case 1 => // Add
            val p1: Long = getVal(modes, pos + 1, 0)
            val p2: Long = getVal(modes, pos + 2, 1)
            val p3: Long = getPos(modes, pos + 3, 2)
            //            println(p3, "=", p1, "+", p2)
            instructions(p3) = p1 + p2
            pos += 4

          case 2 => // Mul
            val p1: Long = getVal(modes, pos + 1, 0)
            val p2: Long = getVal(modes, pos + 2, 1)
            val p3: Long = getPos(modes, pos + 3, 2)
            //            println(p3, "=", p1, "*", p2)
            instructions(p3) = p1 * p2
            pos += 4

          case 3 => // Input
            val p1: Long = getPos(modes, pos + 1, 0)
            //            println(p1, "input", input)
            instructions(p1) = input.head
            input = input.tail
            pos += 2

          case 4 => // Output
            val p1: Long = getVal(modes, pos + 1, 0)
            //            println("output", p1)
            val output: Long = p1
            pos += 2
            return Some(output)

          case 5 => // jump-if-true
            val p1: Long = getVal(modes, pos + 1, 0)
            val p2: Long = getVal(modes, pos + 2, 1)
            //            println("jump-if-true", p1, p2)
            if (p1 != 0) {
              pos = p2
            } else {
              pos += 3
            }

          case 6 => // jump-if-false
            val p1: Long = getVal(modes, pos + 1, 0)
            val p2: Long = getVal(modes, pos + 2, 1)
            //            println("jump-if-false", p1, p2)
            if (p1 == 0) {
              pos = p2
            } else {
              pos += 3
            }

          case 7 => // less than
            val p1: Long = getVal(modes, pos + 1, 0)
            val p2: Long = getVal(modes, pos + 2, 1)
            val p3: Long = getPos(modes, pos + 3, 2)
            //            println("less than", p1, p2, p3)
            if (p1 < p2) {
              instructions(p3) = 1
            } else {
              instructions(p3) = 0
            }
            pos += 4

          case 8 => // equals
            val p1: Long = getVal(modes, pos + 1, 0)
            val p2: Long = getVal(modes, pos + 2, 1)
            val p3: Long = getPos(modes, pos + 3, 2)
            //            println("equals", p1, p2, p3)
            if (p1 == p2) {
              instructions(p3) = 1
            } else {
              instructions(p3) = 0
            }
            pos += 4

          case 9 => // adjusts the relative base
            val p1: Long = getVal(modes, pos + 1, 0)
            //            println("setrel", pos, "+", p1)
            relativeBase += p1
            pos += 2

          case 99 => // Exit
            //            println("exit")
            isHalted = true
            return None

          case _ =>
            println("invalid opcode ", op)
            return None
        }
      }

      None
    }

    private def getVal(modes: Array[Long], pos: Long, modePos: Int): Long = {
      val mode = if (modePos < modes.length) {
        modes(modePos)
      } else {
        0
      }

      mode match {
        case 0 => // Pos
          val p: Long = instructions.getOrElse(pos, 0)
          val v: Long = instructions.getOrElse(p, 0)
          return v
        case 1 => // Im
          val v: Long = instructions.getOrElse(pos, 0)
          return v
        case 2 => // Relative
          val p: Long = instructions.getOrElse(pos, 0)
          val v: Long = instructions.getOrElse(relativeBase + p, 0)
          return v
        case _ =>
          println("invalid mode ", mode)
      }

      -1
    }

    private def getPos(modes: Array[Long], pos: Long, modePos: Int): Long = {
      val mode = if (modePos < modes.length) {
        modes(modePos)
      } else {
        0
      }

      mode match {
        case 0 => // Pos
          val p: Long = instructions.getOrElse(pos, 0)
          return p
        case 2 => // Relative
          val p: Long = instructions.getOrElse(pos, 0)
          return relativeBase + p
        case _ =>
          println("invalid mode ", mode)
      }

      -1
    }
  }

}
