import scala.collection.mutable
import scala.io.Source

object Day17 extends App {
  val file = Source.fromFile("./input/day17")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Long] = lines.split(",").map((str: String) => str.trim.toLong)

  def createSoftware(arg: Long): ControlSoftware = {
    val indexedInstructions: Array[(Long, Long)] = originalInstructions.zipWithIndex.map((tuple: (Long, Int)) => (tuple._2.toLong, tuple._1))
    val mappedInstructions: mutable.Map[Long, Long] = mutable.Map.from(indexedInstructions)
    mappedInstructions(0) = arg
    new ControlSoftware(0, mappedInstructions, Array.empty)
  }

  val software1 = createSoftware(1)

  val map = mutable.Map.empty[(Long, Long), Char]

  def run(software: ControlSoftware): Unit = {
    var x = 0L
    var y = 0L

    while (!software.isHalted) {
      software.run() match {
        case Some(value) =>
          value.toChar match {
            case '.' | '#' | '^' | '>' | 'v' | '<' =>
              map((x, y)) = value.toChar
              x += 1
            case '\n' =>
              y += 1
              x = 0
          }
        case None =>
      }
    }
  }


  def printMap(): Unit = {
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


  def findIntersections(): Unit = {
    val offsets = Array(
      (0L, 1L),
      (1L, 0L),
      (0L, -1L),
      (-1L, 0L)
    )

    val sum = map.filter(kv => kv._2 == '#').map(kv => {
      val pos = kv._1
      val count = offsets.map((offset: (Long, Long)) => map.getOrElse((pos._1 + offset._1, pos._2 + offset._2), '.')).count(c => c == '#')
      if (count >= 4) {
        pos._1 * pos._2
      } else {
        0
      }
    }).sum

    println("result1", sum)
  }

  run(software1)
  //  printMap()
  findIntersections()


  def run2(software: ControlSoftware): Unit = {
    // Hand-Solved
    val routines = "A,A,B,C,B,C,B,C,B,A".split(',').map(s => s.charAt(0))
    val movementFuncs: Array[String] = Array(
      "R,6,L,12,R,6",
      "L,12,R,6,L,8,L,12",
      "R,12,L,10,L,10"
    )

    routines.init.foreach(c => {
      software.input = software.input.appended(c.toInt)
      software.input = software.input.appended(','.toInt)
    })
    software.input = software.input.appended(routines.last)
    software.input = software.input.appended('\n'.toInt)


    movementFuncs.zipWithIndex.foreach((tuple: (String, Int)) => {
      val func = tuple._1
      val splitfunc = func.split(',')
      splitfunc.init.foreach(str => {
        str.foreach(c => {
          software.input = software.input.appended(c.toInt)
        })
        software.input = software.input.appended(','.toInt)
      })

      splitfunc.last.foreach(c => {
        software.input = software.input.appended(c.toInt)
      })

      software.input = software.input.appended('\n'.toInt)
    })

    //    software.input = software.input.appended('y'.toInt) // feed on
    software.input = software.input.appended('n'.toInt) // feed off
    software.input = software.input.appended('\n'.toInt)

    var vals = Array.empty[Long]
    while (!software.isHalted) {
      software.run() match {
        case Some(value) =>
          vals = vals.appended(value)
        //          if (value.isValidChar) {
        //            print(value.toChar)
        //          }
        case None =>
      }
    }


    val last = vals.last
    println("result2", last)
  }

  val software2 = createSoftware(2)
  run2(software2)

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
