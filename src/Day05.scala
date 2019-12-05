import scala.io.Source

object Day05 extends App {
  val file = Source.fromFile("./input/day05")
  val lines = file.mkString
  file.close()
  val originalInstructions: Array[Int] = lines.split(",").map((str: String) => str.trim.toInt)
  //  PrintNums(originalInstructions)

  var instructions = originalInstructions.clone()
  val output1 = Run(instructions, 1)
  println("result1", output1)

  instructions = originalInstructions.clone()
  val output2 = Run(instructions, 5)
  println("result2", output2)


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

  private def Run(instructions: Array[Int], input: Int): Int = {
    var output = 0
    var pos = 0
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
          instructions(p1) = input
          pos += 2

        case 4 => // Output
          val p1 = GetVal(instructions, modes, pos + 1, 0)
          //          println("output", p1)
          output = p1
          pos += 2

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
          return output

        case _ =>
          return output
      }
    }

    output
  }

  //  private def PrintNums(n: Array[Int]): Unit = {
  //    n.foreach((i: Int) => {
  //      print(i)
  //      print(",")
  //    })
  //    println()
  //  }
}