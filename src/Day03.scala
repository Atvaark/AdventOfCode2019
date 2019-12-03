import scala.collection.mutable
import scala.io.Source

object Day03 extends App {
  val file = Source.fromFile("./input/day03")
  val lines = file.getLines.toArray
  file.close()

  val wires: Array[Array[String]] = lines.map((line: String) => {
    line.split(',')
  })

  // pos => [(index, distance),...]
  var map: mutable.Map[(Int, Int), Array[(Int, Int)]] = mutable.Map[(Int, Int), Array[(Int, Int)]]()
  map((0, 0)) = Array((0, 0), (1, 0))

  for ((wire, index) <- wires.view.zipWithIndex) {
    var pos = (0, 0)
    var steps = 0
    wire.foreach((wireInstr: String) => {
      val dir = wireInstr.substring(0, 1)

      val offset = dir match {
        case "U" => (0, 1)
        case "R" => (1, 0)
        case "D" => (0, -1)
        case "L" => (-1, 0)
      }

      val count = wireInstr.substring(1).toInt
      for (_ <- 1 to count) {
        pos = (pos._1 + offset._1, pos._2 + offset._2)
        steps += 1
        val arr: Array[(Int, Int)] = map.getOrElseUpdate(pos, Array.empty)
        val id = arr.indexWhere(tuple => tuple._1 == index)
        if (id < 0) {
          map(pos) = arr.toList.appended((index, steps)).toArray
        } else {
          val value = arr(id)
          if (value._2 > steps) {
            arr(id) = (index, steps)
          }
        }
      }
    })
  }

  var distanceManhattan: Option[Int] = None
  map.foreach((mapElement: ((Int, Int), Array[(Int, Int)])) => {
    if (mapElement._2.length > 1) {
      val distance = math.abs(mapElement._1._1) + math.abs(mapElement._1._2)
      if (distance > 0) {
        distanceManhattan = distanceManhattan match {
          case Some(value) => if (distance < value) Some(distance) else Some(value)
          case None => Some(distance)
        }
      }
    }
  })
  println(distanceManhattan) // part

  var distanceWire: Option[Int] = None
  map.foreach((mapElement: ((Int, Int), Array[(Int, Int)])) => {
    if (mapElement._2.length > 1) {
      val distance = mapElement._2.map(tuple => tuple._2).sum
      if (distance > 0) {
        distanceWire = distanceWire match {
          case Some(value) => if (distance < value) Some(distance) else Some(value)
          case None => Some(distance)
        }
      }
    }
  })
  println(distanceWire) // part2
}