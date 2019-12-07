import scala.io.Source
import scala.collection.mutable

object Day07 extends App {
  val file = Source.fromFile("./input/day07")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Int] = lines.split(",").map((str: String) => str.trim.toInt)

  Loop1(originalInstructions)
  Loop2(originalInstructions)

  private def Loop1(originalInstructions: Array[Int]): Unit = {
    var max1 = 0
    (0 to 4).permutations.foreach((value: IndexedSeq[Int]) => {
      var output = 0
      value.foreach((i: Int) => {
        val software = new ControlSoftware(0, originalInstructions.clone(), Array(i, output))
        software.Run() match {
          case None =>
          case o: Some[Int] =>
            output = o.value
            if (output > max1) {
              max1 = output
            }
        }
      })
    })
    println(max1)
  }

  private def Loop2(originalInstructions: Array[Int]): Unit = {
    var max = 0
    (5 to 9).permutations.foreach((perm: IndexedSeq[Int]) =>  {
      val thrusters = (0 to 4).map(index => new ControlSoftware(index, originalInstructions.clone(), Array(perm(index)))).toList
      var prevOutput = 0
      var cont = true
      while (cont) {
        thrusters.foreach((software: ControlSoftware) => {
          software.input = software.input.appended(prevOutput)
          val out = software.Run()
          out match  {
            case None =>
              cont = false
            case o: Some[Int] =>
              prevOutput = o.value

              if (software.id == 4 &&  o.value > max) {
                max = o.value
              }
          }
        })
      }
    })

    println(max)
  }

  private def GetVal(instructions: Array[Int], modes: Array[Int], pos: Int, modePos: Int): Int = {
    val mode = if (modePos < modes.length) {
      modes(modePos)
    } else {
      0
    }

    mode match {
      case 0 => // Pos
        val p = instructions(pos)
        val v = instructions(p)
        return v
      case 1 => // Im
        val v = instructions(pos)
        return v
      case _ =>
        println("invalid mode ", mode)
    }

    -1
  }

  class ControlSoftware(val id: Int, var instructions: Array[Int], var input: Array[Int]) {
    private var pos = 0

    def Run(): Option[Int] = {
      while (pos < instructions.length) {
        val instruction = instructions(pos)
        val op = instruction % 10

        val modes = if (instruction.toString.length >= 2) {
          instruction.toString.reverse.substring(2).map(c => c.toString.toInt).toArray
        } else {
          Array[Int]()
        }

        op match {
          case 1 => // Add
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            val p2 = GetVal(instructions, modes, pos + 2, 1)
            //          val p3 = GetVal(instructions, modes, pos + 3, 2)
            val p3 = instructions(pos + 3)
            //          println(p3, "=", p1, "+", p2)
            instructions(p3) = p1 + p2
            pos += 4

          case 2 => // Mul
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            val p2 = GetVal(instructions, modes, pos + 2, 1)
            //          val p3 = GetVal(instructions, modes, pos + 3, 2)
            val p3 = instructions(pos + 3)
            //          println(p3, "=", p1, "*", p2)
            instructions(p3) = p1 * p2
            pos += 4

          case 3 => // Input
            val p1 = instructions(pos + 1)
            //          println(p1, "input", input)
            instructions(p1) = input.head
            input = input.tail
            pos += 2

          case 4 => // Output
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            //          println("output", p1)
            val output = p1
            pos += 2
            return Some(output)

          case 5 => // jump-if-true
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            val p2 = GetVal(instructions, modes, pos + 2, 1)
            //          println("jump-if-true", p1, p2)
            if (p1 != 0) {
              pos = p2
            } else {
              pos += 3
            }

          case 6 => // jump-if-false
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            val p2 = GetVal(instructions, modes, pos + 2, 1)
            //          println("jump-if-false", p1, p2)
            if (p1 == 0) {
              pos = p2
            } else {
              pos += 3
            }

          case 7 => // less than
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            val p2 = GetVal(instructions, modes, pos + 2, 1)
            val p3 = instructions(pos + 3)
            //          println("less than", p1, p2, p3)
            if (p1 < p2) {
              instructions(p3) = 1
            } else {
              instructions(p3) = 0
            }
            pos += 4

          case 8 => // equals
            val p1 = GetVal(instructions, modes, pos + 1, 0)
            val p2 = GetVal(instructions, modes, pos + 2, 1)
            val p3 = instructions(pos + 3)
            //          println("equals", p1, p2, p3)
            if (p1 == p2) {
              instructions(p3) = 1
            } else {
              instructions(p3) = 0
            }
            pos += 4

          case 99 => // Exit
            //          println("exit")
            return None

          case _ =>
            println("invalid opcode ", op)
            return None
        }
      }

      None
    }
  }
}