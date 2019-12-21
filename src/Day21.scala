import scala.collection.mutable
import scala.io.Source

object Day21 extends App {
  val file = Source.fromFile("./input/day21")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Long] = lines.split(",").map((str: String) => str.trim.toLong)

  def createSoftware(): ControlSoftware = {
    val indexedInstructions: Array[(Long, Long)] = originalInstructions.zipWithIndex.map((tuple: (Long, Int)) => (tuple._2.toLong, tuple._1))
    val mappedInstructions: mutable.Map[Long, Long] = mutable.Map.from(indexedInstructions)
    new ControlSoftware(0, mappedInstructions, Array.empty)
  }

  part1()
  part2()

  def part1(): Unit = {
    val script = Array(
      "OR A J",
      "AND B J",
      "AND C J",
      "NOT J J",
      "AND D J",
      "WALK"
    )

    run1(script) match {
      case Some(value) =>
        println("result1", value)
      case None =>
    }
  }

  def part2(): Unit = {
    val script = Array(
      "NOT A J",
      "NOT B T",
      "OR T J",
      "NOT C T",
      "OR T J",
      "AND D J",
      "NOT H T",
      "NOT T T",
      "OR E T",
      "AND T J",
      "RUN"
    )

    run1(script) match {
      case Some(value) =>
        println("result2", value)
      case None =>
    }
  }

  //  def bruteforce(): Unit = {
  //    val instr = Array("AND", "OR", "NOT")
  //    val inregs = Array("A","B", "C", "D")
  //    val outregs = Array("T", "J")
  //    val regs  = inregs.concat(outregs).toArray
  //    val maxlen = 15
  //
  //    val combinations: Array[(String, String, String)] = (for (i <- instr; r1 <- regs; r2 <- outregs) yield (i, r1, r2)).toArray
  //
  //    def comb1(k: Int): Seq[Seq[String]] = {
  //      comb2(Seq.empty, combinations.length, k)
  //    }
  //
  //    def comb2(prefix: Seq[String], n: Int, k: Int): Seq[Seq[String]] = {
  //      if (k == 0) {
  //        return Seq(prefix)
  //      }
  //      val aeq = (0 until n).map(i => {
  //        val t: (String, String, String) = combinations(i)
  //        val ts = Array(t._1, t._2, t._3).mkString(" ")
  //        val newPrefix: Seq[String] = prefix.appended(ts)
  //        comb2(newPrefix, n, k - 1)
  //      })
  //      aeq.flatten
  //    }
  //
  //    for (len <- 1 to maxlen) {
  //      println(s"len $len")
  //      val scripts = comb1(len).toArray
  //      for (script <- scripts) {
  //
  //        run1(script) match {
  //          case Some(value) =>
  //            println("script", script)
  //            println("result", value)
  //          case None =>
  ////            println("mismatch")
  //        }
  //      }
  //    }
  //  }


  def run1(script: Seq[String]): Option[Long] = {
    val software = createSoftware()
    var input = Array.empty[Long]
    script.foreach(line => {
      line.foreach(char => {
        input = input.appended(char.toInt)
      })
      input = input.appended('\n'.toInt)
    })

    software.input = input

    var result = Seq.empty[Char]
    var damage = 0L
    while (!software.isHalted) {
      software.run() match {
        case Some(value) =>
          damage = value
          result = result.appended(value.toChar)
        case None =>
      }
    }


    val resultString = result.mkString

    if (resultString.contains("Didn't make it across")
      || resultString.contains("Invalid second argument")
      || resultString.contains("Out of memory")
    ) {
      println(resultString)
      None
    } else {
      Some(damage)
    }
  }

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
