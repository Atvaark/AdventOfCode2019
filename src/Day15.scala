import scala.collection.mutable
import scala.io.Source

object Day15 extends App {
  val file = Source.fromFile("./input/day15")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Long] = lines.split(",").map((str: String) => str.trim.toLong)
  val indexedInstructions: Array[(Long, Long)] = originalInstructions.zipWithIndex.map((tuple: (Long, Int)) => (tuple._2.toLong, tuple._1))
  val mappedInstructions: mutable.Map[Long, Long] = mutable.Map.from(indexedInstructions)
  val software = new ControlSoftware(0, mappedInstructions, Array.empty)

  // None = Unknown
  // 0 = Wall
  // 1 = Free
  // 2 = Goal
  val grid = mutable.Map.empty[(Long, Long), (Long, Long)]
  val startPos = (0L, 0L)
  grid(startPos) = (0L, 0L) // dist, type

  def printGrid(): Unit = {

    val minX = grid.keys.map(pos => pos._1).min
    val maxX = grid.keys.map(pos => pos._1).max
    val minY = grid.keys.map(pos => pos._2).min
    val maxY = grid.keys.map(pos => pos._2).max

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        grid.get((x, y)) match {
          case Some(value) =>
            print(value._2)
          case None =>
            print(' ')
        }
      }
      println()
    }
    println()
  }

  //  grid(pos) = 1
  var start = ((0L, 0L), (0L, software))
  private val queue = mutable.PriorityQueue[((Long, Long), (Long, ControlSoftware))](start)(Ordering.by(
    (tuple: ((Long, Long), (Long, ControlSoftware))) => tuple._2._1))

  while (queue.nonEmpty) {
    val current: ((Long, Long), (Long, ControlSoftware)) = queue.dequeue()

    val offsets = Array(
      ((0L, -1L), 1), // N
      ((0L, 1L), 2), // S
      ((-1L, 0L), 3), // W
      ((1L, 0L), 4) // E
    )

    offsets.foreach((offset: ((Long, Long), Int)) => {
      val software = current._2._2.clone().asInstanceOf[ControlSoftware]

      software.input = Array(offset._2)
      software.run() match {
        case Some(responseCode) =>
          val nextPos = (current._1._1 + offset._1._1, current._1._2 + offset._1._2)
          val nextDist = current._2._1 + 1
          if (responseCode != 0) {
            if (!grid.contains(nextPos)) {
              queue.enqueue((nextPos, (nextDist, software)))
            }
          }

          grid(nextPos) = (nextDist, responseCode)
        case None =>
          println("huh")
      }
    })
  }


  //  printGrid

  // part 1
  private val oxygen: ((Long, Long), (Long, Long)) = grid.filter(kv => kv._2._2 == 2).head
  val distanceToOxy = oxygen._2._1
  println(distanceToOxy) // distance from start to oxygen

  // part 2
  var oxygenPos = oxygen._1
  val oxyQueue = mutable.Queue.empty[(Long, Long, Long)]
  oxyQueue.enqueue((oxygenPos._1, oxygenPos._2, 0))
  var oxyDiscovered = mutable.Set.empty[(Long, Long)]
  oxyDiscovered.add(oxygenPos)
  var minutesToFill = 0L
  while (oxyQueue.nonEmpty) {
    val current = oxyQueue.dequeue()
    if (current._3 > minutesToFill) {
      minutesToFill = current._3
    }
    val offsets = Array(
      (0L, -1L), // N
      (0L, 1L), // S
      (-1L, 0L), // W
      (1L, 0L) // E
    )
    val nextDist = current._3 + 1
    offsets.map((offset: (Long, Long)) => {
      val nextPos = (current._1 + offset._1, current._2 + offset._2)
      nextPos
    }).filter(pos => {
      if (oxyDiscovered.contains(pos)) {
        false
      } else {
        val gridVal = grid(pos)
        gridVal._2 > 0
      }
    }).foreach((adjacent: (Long, Long)) => {
      oxyDiscovered.add(adjacent)
      oxyQueue.enqueue((adjacent._1, adjacent._2, nextDist))
    })
  }
  println(minutesToFill)


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
