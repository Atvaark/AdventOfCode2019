import scala.collection.mutable
import scala.io.Source
import util.control.Breaks._

object Day19 extends App {
  val file = Source.fromFile("./input/day19")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Long] = lines.split(",").map((str: String) => str.trim.toLong)

  def createSoftware(): ControlSoftware = {
    val indexedInstructions: Array[(Long, Long)] = originalInstructions.zipWithIndex.map((tuple: (Long, Int)) => (tuple._2.toLong, tuple._1))
    val mappedInstructions: mutable.Map[Long, Long] = mutable.Map.from(indexedInstructions)
    //    mappedInstructions(0) = arg
    new ControlSoftware(0, mappedInstructions, Array.empty)
  }

  val originalSoftware = createSoftware()

  def check(pos: (Long, Long)): Boolean = {
    val software = originalSoftware.clone().asInstanceOf[ControlSoftware]
    software.input = Array(pos._1, pos._2)
    software.run() match {
      case Some(value) =>
        value == 1
      case None =>
        false
    }
  }

  def run1(software: ControlSoftware): Unit = {
    var sum = 0L
    for (x <- 0L until 50L) {
      for (y <- 0L until 50L) {
        if (check((x, y))) {
          sum += 1
        }
      }
    }

    println("result1", sum)
  }


  def run2(): Unit = {

    val req = 100L
    var minx = req
    var miny = 0L
    val max = Int.MaxValue.toLong - 1

    for (x <- minx to max) {
      minx = x

      breakable {
        for (y <- miny to max) {
          val pos1 = (x, y)
          if (check(pos1)) {
            miny = y

            val pos2 = (x - req + 1, y + req - 1)
            //            println(s"check $pos1 + $pos2")
            if (check(pos2)) {
              val pos3 = (x - req + 1, y)
              //              println(s"found $pos2 $pos3")

              val res = (pos3._1 * 10000) + pos3._2
              println("result2", res)
              return
            }

            break()
          }
        }
      }
    }






    //    printMap(map.toMap)
    //    println()

    // goal: find 100x100 square that fits in beam closest to 0,0
    // optimization: only check y below equal latest/max
    //                          x greater equal latest/max
    // print top left x*10000 + y
  }


  def printMap(map: Map[(Long, Long), Long]): Unit = {
    val minX = map.keys.map(pos => pos._1).min
    val maxX = map.keys.map(pos => pos._1).max
    val minY = map.keys.map(pos => pos._2).min
    val maxY = map.keys.map(pos => pos._2).max

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        val v = map.getOrElse((x, y), '.')
        print(v)
      }
      println()
    }
    println()
  }

  run1(originalSoftware) // 199
  run2()


  class ControlSoftware(val id: Long, var instructions: mutable.Map[Long, Long], var input: Array[Long]) {
    private var pos: Long = 0
    private var relativeBase: Long = 0
    var isHalted = false

    override def clone(): AnyRef = {
      val other = new ControlSoftware(
        id,
        instructions.clone(),
        input.clone()
      )
      other.pos = pos
      other.relativeBase = relativeBase
      other.isHalted = isHalted
      other
    }

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
