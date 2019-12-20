
import scala.collection.mutable
import scala.io.Source

object Day20 extends App {
  val file = Source.fromFile("./input/day20")
  val lines = file.getLines().toArray
  file.close()


  var map = mutable.Map[(Int, Int), Char]()

  lines.zipWithIndex.foreach((row: (String, Int)) => {
    row._1.zipWithIndex.foreach((column: (Char, Int)) => {
      column._1 match {
        case ' ' =>
        case _ =>
          map((column._2, row._2)) = column._1
      }
    })
  })


  val l = genLookup()
  val lookup = l._1
  val outerPortalsLookup = l._2
  val start = l._3
  val end = l._4

  part1()

  part2()

  def part1(): Unit = {
    class Node(val pos: (Int, Int), val dist: Int, val prev: Node)
    val initial = new Node(start, 0, null)
    val queue = mutable.PriorityQueue.empty[Node](
      Ordering.by((node: Node) => node.dist).reverse
    )
    queue.enqueue(initial)
    val visited = mutable.Map(start -> 0)
    var min = Int.MaxValue
    while (queue.nonEmpty) {
      val current = queue.dequeue()
      if (current.pos == end) {
        if (current.dist < min) {
          min = current.dist
        }
      }

      Array(
        (1, 0),
        (-1, 0),
        (0, 1),
        (0, -1)
      ).foreach(offset => {
        val newPos = (current.pos._1 + offset._1, current.pos._2 + offset._2)
        val newDist = current.dist + 1
        map(newPos) match {
          case '#' =>
          case '.' =>
            if (!visited.contains(newPos) || visited(newPos) > newDist) {
              visited(newPos) = newDist
              val newNode = new Node(newPos, newDist, current)
              queue.enqueue(newNode)
            }
          case _ =>
            lookup.get(newPos) match {
              case Some(portalTargetPos) =>
                if (!visited.contains(portalTargetPos) || visited(portalTargetPos) > newDist) {
                  visited(portalTargetPos) = newDist
                  val newNode = new Node(portalTargetPos, newDist, current)
                  queue.enqueue(newNode)
                }
              case _ =>
            }
        }
      })
    }

    println("part1", min)
  }

  def part2(): Unit = {
    class Node(val pos: (Int, Int), val layer: Int, val dist: Int, val prev: Node)
    val initial = new Node(start, 0, 0, null)
    val queue = mutable.PriorityQueue.empty[Node](
      Ordering.by((node: Node) => node.layer).reverse
    )
    queue.enqueue(initial)
    val visited = mutable.Set((start, 0))

    var min = Int.MaxValue
    while (queue.nonEmpty) {
      val current = queue.dequeue()

      if (current.pos == end && current.layer == 0) {
        if (current.dist < min) {
          min = current.dist
          queue.clear()
        }
      }

      Array(
        (1, 0),
        (-1, 0),
        (0, 1),
        (0, -1)
      ).foreach(offset => {
        val newPos = (current.pos._1 + offset._1, current.pos._2 + offset._2)
        var newLayer = current.layer
        val newDist = current.dist + 1
        map(newPos) match {
          case '#' =>
          case '.' =>
            if (!visited.contains((newPos, newLayer))) {
              visited.add(newPos, newLayer)
              val newNode = new Node(newPos, current.layer, newDist, current)
              queue.enqueue(newNode)
            }
          case _ =>
            lookup.get(newPos) match {
              case Some(portalTargetPos) =>
                val outer = outerPortalsLookup(newPos)
                var continue = false
                if (!outer) {
                  newLayer = current.layer + 1
                  continue = true
                } else if (current.layer > 0) {
                  newLayer = current.layer - 1
                  continue = true
                }

                if (continue) {

                  if (!visited.contains(portalTargetPos, newLayer)) {
                    visited.add((portalTargetPos, newLayer))
                    val newNode = new Node(portalTargetPos, newLayer, newDist, current)
                    queue.enqueue(newNode)
                  }
                }

              case _ =>
            }
        }
      })
    }

    println("part", min)
  }

  def findStart(c: Char): (Int, Int) = {
    val positions = map.filter(kv => kv._2 == c).toArray.map(kv => kv._1)
    for (position <- positions) {
      Array(
        (1, 0),
        (-1, 0),
        (0, 1),
        (0, -1)
      ).foreach(offset => {
        val position2 = (position._1 + offset._1, position._2 + offset._2)
        map.getOrElse(position2, ' ') match {
          case '.' =>
            return position2
          case _ =>
        }
      })
    }

    println(s"could not find char '$c''")
    (-1, -1)
  }

  def genLookup(): (Map[(Int, Int), (Int, Int)], Map[(Int, Int), Boolean], (Int, Int), (Int, Int)) = {

    var items = List.empty[(String, (Int, Int), (Int, Int), (Int, Int))]

    map.filter(kv => kv._2.isLetter).foreach(kv => {
      val offsets = Array(
        (1, 0),
        (0, 1)
      )

      offsets.foreach(offset => {
        val position1 = kv._1
        val position2 = (position1._1 + offset._1, position1._2 + offset._2)
        map.getOrElse(position2, ' ') match {
          case ' ' | '#' | '.' =>
          case portal =>
            val key = Array(kv._2, portal).mkString
            val position3 = (position1._1 - offset._1, position1._2 - offset._2)
            val position4 = (position2._1 + offset._1, position2._2 + offset._2)
            Array(
              position3,
              position4
            ).filter(pos => map.getOrElse(pos, ' ') == '.').foreach(position5 => {
              val item = (key, position1, position2, position5)
              items = items.appended(item)
            })
        }
      })
    })

    val minX = map.keys.map(pos => pos._1).min
    val maxX = map.keys.map(pos => pos._1).max
    val minY = map.keys.map(pos => pos._2).min
    val maxY = map.keys.map(pos => pos._2).max

    var start = (-1, -1)
    var end = (-1, -1)
    val startToEndPortals = mutable.Map.empty[(Int, Int), (Int, Int)]
    val isOuterLayerPortal = mutable.Map.empty[(Int, Int), Boolean]
    items.groupBy(i => i._1).foreach(g => {
      if (g._2.size == 1) {
        // start / end
        g._1 match {
          case "AA" =>
            start = g._2.head._4
          case "ZZ" =>
            end = g._2.head._4
        }


      } else {
        g._2.zipWithIndex.foreach(i => {
          val other = g._2((i._2 + 1) % g._2.size)
          startToEndPortals(i._1._2) = other._4
          startToEndPortals(i._1._3) = other._4

          def isOuter(pos: (Int, Int)): Boolean = {
            val x = pos._1
            if (x == minX || x == minX + 1 || x == maxX || x == maxX - 1) {
              return true
            }

            val y = pos._2
            if (y == minY || y == minY + 1 || y == maxY || y == maxY - 1) {
              return true
            }

            false
          }

          isOuterLayerPortal(i._1._2) = isOuter(i._1._2)
          isOuterLayerPortal(i._1._3) = isOuter(i._1._3)
        })
      }


    })

    (startToEndPortals.toMap, isOuterLayerPortal.toMap, start, end)
  }

  def printMap(map: Map[(Int, Int), Char]): Unit = {
    val minX = map.keys.map(pos => pos._1).min
    val maxX = map.keys.map(pos => pos._1).max
    val minY = map.keys.map(pos => pos._2).min
    val maxY = map.keys.map(pos => pos._2).max

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        print(map.getOrElse((x, y), ' '))
      }
      println()
    }
    println()
  }


}
