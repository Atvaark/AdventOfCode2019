import scala.collection.mutable
import scala.io.Source

object Day14 extends App {
  val file = Source.fromFile("./input/day14")
  val lines = file.getLines.toArray
  file.close()

  val lineRegex = """^(.*?)=>(.*?)$""".r
  val itemRegex = """(\d+)\s+(\w+)""".r

  class ChemicalQuantity(val count: Long, val chemical: String) {
  }

  class Reaction(val input: Array[ChemicalQuantity], var output: ChemicalQuantity) {
  }

  val reactions = lines.map((line: String) => {
    line match {
      case lineRegex(in, out) =>
        val inChem = in.split(',').map((str: String) => {
          str.trim match {
            case itemRegex(count, chemical) =>
              new ChemicalQuantity(count.toLong, chemical)
          }
        })
        val outChem = out.trim match {
          case itemRegex(count, chemical) =>
            new ChemicalQuantity(count.toLong, chemical)
        }
        new Reaction(inChem, outChem)
    }
  })

  val finalReaction: Reaction = reactions.filter(r => r.output.chemical == "FUEL").head


  var excessChemicals = mutable.Map.empty[String, Long].withDefault((_: String) => 0)

  def getOreRequired(chemical: String, required: Long): Long = {
    if (chemical == "ORE") {
      return required
    }

    //    println(excessChemicals)

    val reaction: Reaction = reactions.filter(r => r.output.chemical == chemical).head

    var requiredCount = required
    var excessAmount = excessChemicals(chemical)

    //    if (chemical == "MZWV" && required == 3) {
    //      println()
    //    }
    //    var tmp = excessAmount
    if (excessAmount >= requiredCount) {
      excessAmount -= requiredCount
      excessChemicals(chemical) = excessAmount
      requiredCount = 0L
    } else {
      // excess < requiredCount
      requiredCount -= excessAmount
      excessAmount = 0L
      excessChemicals(chemical) = excessAmount
    }

    val outputAmount = reaction.output.count
    var repetition: Long = requiredCount / outputAmount
    if (requiredCount % outputAmount > 0L) {
      repetition += 1
    }

    val outputExcess = (outputAmount * repetition) - requiredCount

    var oreCount = 0L
    reaction.input.foreach((input: ChemicalQuantity) => {
      val inputOreRequired: Long = getOreRequired(input.chemical, input.count * repetition)
      oreCount += inputOreRequired
    })

    excessChemicals(chemical) = excessChemicals(chemical) + outputExcess

    oreCount
  }


  def getFuelProduced: Long = {
    val oreLimit = 1000000000000L

    var lower = 0L
    var upper = Long.MaxValue

    var continue = true
    var fuelCount = 1L
    while (continue) {
      excessChemicals.clear()
      val oreReq = getOreRequired("FUEL", fuelCount)
      if (oreReq < oreLimit) {
        lower = fuelCount
      } else {
        upper = fuelCount
        continue = false
      }

      fuelCount = fuelCount * 2
    }

    while (true) {
      val mid = lower + ((upper - lower) / 2L)
      excessChemicals.clear()
      val oreReq = getOreRequired("FUEL", mid)
      if (oreReq < oreLimit) {
        lower = mid
      } else if (oreReq > oreLimit) {
        upper = mid
      } else {
        return mid
      }

      if (upper - lower <= 1) {
        return lower
      }
    }

    0
  }


  val result1 = getOreRequired("FUEL", 1)
  println("result1", result1)

  val result2 = getFuelProduced
  println("result2", result2)
}