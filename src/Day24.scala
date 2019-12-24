import scala.io.Source

object Day24 extends App {
  val file = Source.fromFile("./input/day24")
  val lines = file.getLines().toArray
  file.close()

  var mapOriginal = Map.empty[(Int, Int), Char]
  var multMap = Map.empty[(Int, Int), Int]

  def parse(): Unit = {
    var e = 0
    lines.zipWithIndex.foreach(line => {
      line._1.zipWithIndex.foreach(char => {
        mapOriginal += ((line._2, char._2) -> char._1)
        multMap += ((line._2, char._2) -> Math.pow(2, e).toInt)
        e += 1
      })
    })

    //    mapOriginal -= ((2, 2))
  }

  parse()

  def getRating(map: Map[(Int, Int), Char]): Int = {
    val sum = map.map(kv => {
      if (kv._2 == '#') {
        multMap(kv._1)
      } else {
        0
      }
    }).sum

    sum
  }

  def printMap(map: Map[(Int, Int), Char]): Unit = {
    val minX = map.keys.map(pos => pos._1).min
    val maxX = map.keys.map(pos => pos._1).max
    val minY = map.keys.map(pos => pos._2).min
    val maxY = map.keys.map(pos => pos._2).max

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        print(map.getOrElse((x, y), '.'))
      }
      println()
    }
    println()
  }


  def part1: Int = {
    var part1 = true
    var map = mapOriginal

    var ratings = Set.empty[Int]
    var offsets = Array(
      (0, 1),
      (1, 0),
      (0, -1),
      (-1, 0)
    )

    while (true) {
      //      printMap(map)

      var next = Map.empty[(Int, Int), Char]
      map.foreach(kv => {
        val bugCount = offsets.map(offset => {
          map.getOrElse((kv._1._1 + offset._1, kv._1._2 + offset._2), '.')
        }).count(tile => tile == '#')

        val nextTile = kv._2 match {
          case '#' =>
            bugCount match {
              case 1 => '#'
              case _ => '.'
            }

          case '.' =>
            bugCount match {
              case 1 | 2 => '#'
              case _ => '.'
            }
        }
        next += (kv._1 -> nextTile)
      })

      map = next

      var r = getRating(map)
      if (ratings.contains(r)) {
        return r
      }

      ratings += r
    }

    0
  }

  var result1 = part1
  println("part1", result1)

  val im = Map(
    (2, 1) -> Array((0, 0), (1, 0), (2, 0), (3, 0), (4, 0)), // ^
    (3, 2) -> Array((4, 0), (4, 1), (4, 2), (4, 3), (4, 4)), // >
    (2, 3) -> Array((0, 4), (1, 4), (2, 4), (3, 4), (4, 4)), // v
    (1, 2) -> Array((0, 0), (0, 1), (0, 2), (0, 3), (0, 4)), // <
  )

  def getInsideCount(pos: (Int, Int), m: Map[(Int, Int), Char]): Int = {
    im(pos).map(p => {
      m.getOrElse(p, '.')
    }).count(c => c == '#')
  }


  // pos inside -> positions outside
  val om = Map(
    // ^
    (0, -1) -> (2, 1),
    (1, -1) -> (2, 1),
    (2, -1) -> (2, 1),
    (3, -1) -> (2, 1),
    (4, -1) -> (2, 1),

    // >
    (5, 0) -> (3, 2),
    (5, 1) -> (3, 2),
    (5, 2) -> (3, 2),
    (5, 3) -> (3, 2),
    (5, 4) -> (3, 2),

    // v
    (4, 5) -> (2, 3),
    (3, 5) -> (2, 3),
    (2, 5) -> (2, 3),
    (1, 5) -> (2, 3),
    (0, 5) -> (2, 3),

    // <
    (-1, 4) -> (1, 2),
    (-1, 3) -> (1, 2),
    (-1, 2) -> (1, 2),
    (-1, 1) -> (1, 2),
    (-1, 0) -> (1, 2),
  )

  def getOutsideCount(pos: (Int, Int), m: Map[(Int, Int), Char]): Int = {
    if (m.getOrElse(om(pos), '.') == '#') 1 else 0
  }

  def createEmptyLayer: Map[(Int, Int), Char] = {
    var m = Map.empty[(Int, Int), Char]
    for (x <- 0 until 5) {
      for (y <- 0 until 5) {
        if ((x, y) != (2, 2)) {
          m += ((x, y) -> '.')
        }
      }
    }

    m
  }

  def part2: Unit = {


    var layers: Map[Int, Map[(Int, Int), Char]] = Map(0 -> mapOriginal)

    val mins = 200
    for (_ <- 1 to mins) {
      var nextLayers = Map.empty[Int, Map[(Int, Int), Char]]

      var nextKeys = layers.keys.toList.sorted.toArray
      nextKeys = Array(nextKeys.min - 1).concat(nextKeys).concat(Array(nextKeys.max + 1))

      for (key <- nextKeys) {
        val layer = layers.getOrElse(key, createEmptyLayer)
        //        val layer = layers.getOrElse(key, Map.empty[(Int, Int), Char])
        var nextLayer = Map.empty[(Int, Int), Char]

        val offsets = Array(
          (0, 1),
          (1, 0),
          (0, -1),
          (-1, 0)
        )

        layer.foreach(kv => {
          val curPos = kv._1
          curPos match {
            case (2, 2) =>
            // skip
            case _ =>
              val bugCount: Int = offsets.map(offset => {

                val nextPos = (curPos._1 + offset._1, curPos._2 + offset._2)


                val c: Int = if (layer.contains(nextPos)) {
                  // current layer
                  if (layer(nextPos) == '#') {
                    1
                  } else {
                    0
                  }
                } else {

                  if (nextPos == (2, 2)) {
                    if (layers.contains(key + 1)) {
                      getInsideCount(curPos, layers(key + 1))
                    } else {
                      // empty inside layer
                      0
                    }
                  } else {
                    if (layers.contains(key - 1)) {
                      getOutsideCount(nextPos, layers(key - 1))
                    } else {
                      // empty outside layer
                      0
                    }
                  }

                }

                c
              }).sum

              val nextTile = kv._2 match {
                case '#' =>
                  bugCount match {
                    case 1 => '#'
                    case _ => '.'
                  }

                case '.' =>
                  bugCount match {
                    case 1 | 2 => '#'
                    case _ => '.'
                  }
              }

              nextLayer += (kv._1 -> nextTile)
          }
        })

        nextLayers += (key -> nextLayer)
      }

      layers = nextLayers
    }

    val bugSum = layers.map(l => {
      l._2.count(ll => ll._2 == '#')
    }).sum

    println("part2", bugSum)
  }

  part2
}
