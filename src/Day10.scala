import scala.collection.mutable
import scala.io.Source

object Day10 extends App {
  val file = Source.fromFile("./input/day10")
  val input = file.getLines().toArray
  file.close()

  var map = mutable.Map[(Int, Int), Option[Int]]()

  input.zipWithIndex.foreach((row: (String, Int)) => {
    row._1.zipWithIndex.foreach((column: (Char, Int)) => {
      column._1 match {
        case '#' =>
          map((column._2, row._2)) = None
        case _ =>
      }
    })
  })

  println(map)


  val minX = map.keys.minBy((coord: (Int, Int)) => coord._1)._1
  val maxX = map.keys.maxBy((coord: (Int, Int)) => coord._1)._1
  val minY = map.keys.minBy((coord: (Int, Int)) => coord._2)._2
  val maxY = map.keys.maxBy((coord: (Int, Int)) => coord._2)._2

  var maxVisible = 0
  var max: (Int, Int) = (-1, -1)

  map.keys.foreach((self: (Int, Int)) => {
    val visible = map.keys.filter(other => other != self).groupBy((other: (Int, Int)) => {
      val dx: Double = self._1 - other._1
      val dy: Double = self._2 - other._2
      val tan = Math.atan2(dx, dy)
      tan
    }).count((tuple: (Double, Iterable[(Int, Int)])) => true)

    if (visible > maxVisible) {
      maxVisible = visible
      max = self
    }
  })

  println("result1", maxVisible)

  private val otherToTan: Map[(Int, Int), Double] = map.keys.filter(other => other != max).map((other: (Int, Int)) => {
    val dx: Double = max._1 - other._1
    val dy: Double = max._2 - other._2
    val tan = Math.atan2(dx, dy)
    (other, tan)
  }).toMap

  var anglesLookup: Map[Double, Seq[((Int, Int), Double)]] = otherToTan.groupBy(_._2).map((group: (Double, Map[(Int, Int), Double])) => {
    val otherSortedByDist = group._2.keys.map((other: (Int, Int)) => {
      val distance = Math.sqrt(Math.pow(max._1 - other._1, 2) + Math.pow(max._2 - other._2, 2))
      (other, distance)
    }).toList
      .sortBy(_._2)
      .toArray
    (group._1, otherSortedByDist)
  })


  var count = 200

  // sort angles clockwise
  val angles: List[Double] = anglesLookup.keys.toList.sortBy((angle: Double) => {
    if (angle == 0) {
      0 // straight up = 0
    } else if (angle < 0) {
      Math.abs(angle) // right = 1 to PI
    } else {
      (2 * Math.PI) - angle // left = PI TO 2PI
    }
  })
  var lastAsteroid = (-1, -1)
  while (anglesLookup.nonEmpty) {
    angles.foreach((angle: Double) => {
      anglesLookup.get(angle) match {
        case Some(value) =>
          if (value.tail.isEmpty) {
            anglesLookup = anglesLookup - angle
          } else {
            anglesLookup = anglesLookup + (angle -> value.tail)
          }

          count -= 1
          if (count == 0) {
            lastAsteroid = value.head._1
          }
        case None =>
      }

    })
  }

  var result2 = (lastAsteroid._1 * 100) + lastAsteroid._2
  println("result2", result2)
}