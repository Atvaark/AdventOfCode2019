import scala.collection.mutable
import scala.io.Source
import util.control.Breaks._

object Day18 extends App {
  val file = Source.fromFile("./input/day18")
  val input = file.getLines().toArray
  file.close()

  val map = mutable.Map.empty[(Int, Int), Char]
  for (y <- input.zipWithIndex) {
    for (x <- y._1.zipWithIndex) {
      map((x._2, y._2)) = x._1
    }
  }


  def printMap(map: Map[(Int, Int), Char]): Unit = {
    val minX = map.keys.map(pos => pos._1).min
    val maxX = map.keys.map(pos => pos._1).max
    val minY = map.keys.map(pos => pos._2).min
    val maxY = map.keys.map(pos => pos._2).max

    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        print(map(x, y))
      }
      println()
    }
    //    println()
  }

  val startPos = map.filter(kv => kv._2 == '@').head._1
  val doorLookup: Map[Char, (Int, Int)] = map.filter(kv => kv._2.isLetter && kv._2.isUpper).map(kv => kv.swap).toMap
  val keyLookup: Map[Char, (Int, Int)] = map.filter(kv => kv._2.isLetter && kv._2.isLower).map(kv => kv.swap).toMap
  val keyChars: List[Char] = keyLookup.keys.toList.sorted
  val keyInts: Map[Char, Int] = keyChars.zipWithIndex.map(n => (n._1, 1 << n._2)).toMap
  val keyIntsSum = keyInts.values.sum

  part1()

  def part1(): Unit = {
    val nodes: Array[Char] = Array('@').concat(doorLookup.keys).concat(keyLookup.keys)
    val nodeLocations = mutable.Map('@' -> startPos)
    nodeLocations.addAll(doorLookup)
    nodeLocations.addAll(keyLookup)

    val nodeDistances = mutable.Map.empty[(Char, Char), Int]
    var visitedPositions = mutable.Set.empty[(Int, Int)]
    val nodeQueue = mutable.Queue.empty[((Int, Int), Int)]

    val offsets = Array(
      (0, -1),
      (1, 0),
      (0, 1),
      (-1, 0),
    )

    for (node <- nodes) {
      visitedPositions = mutable.Set.empty[(Int, Int)]

      val startPos = nodeLocations(node)
      nodeQueue.enqueue((startPos, 0))
      visitedPositions(startPos) = true

      while (nodeQueue.nonEmpty) {
        val tuple = nodeQueue.dequeue()
        val currentPos = tuple._1
        val currentDistance = tuple._2
        val currentChar = map(currentPos)
        if (nodes.contains(currentChar) && currentChar != node) {
          nodeDistances((node, currentChar)) = currentDistance
        } else {
          offsets.foreach(offset => {
            val newPos = (currentPos._1 + offset._1, currentPos._2 + offset._2)
            if (map(newPos) != '#') {
              if (!visitedPositions.contains(newPos)) {
                visitedPositions(newPos) = true
                nodeQueue.enqueue((newPos, currentDistance + 1))
              }
            }
          })
        }
      }
    }

    val charKeyDistances = mutable.Map.empty[(Char, Int), Int]

    val queue = mutable.PriorityQueue.empty[(Int, Int, Char)](
      Ordering.by((_: (Int, Int, Char))._1).reverse
    )
    queue.enqueue((0, 0, '@'))
    charKeyDistances(('@', 0)) = 0

    def smaller(newDist: Int, node: Char, intKeyMap: Int): Boolean = {
      if (!charKeyDistances.contains((node, intKeyMap))) {
        true
      } else {
        newDist < charKeyDistances((node, intKeyMap))
      }
    }

    var minValue = Int.MaxValue
    while (queue.nonEmpty) {
      val tuple = queue.dequeue()
      val currentDistance = tuple._1
      val currentKeyInt = tuple._2
      val currenNode = tuple._3
      if (currentKeyInt == keyIntsSum) {
        minValue = currentDistance
        queue.clear()
      } else {
        val nextNodes = nodeDistances.filter(n => n._1._1 == currenNode).map(n => n._1._2)
        nextNodes.foreach(nextNode => {
          val newDistance = currentDistance + nodeDistances((currenNode, nextNode))
          var nextKeyInt = currentKeyInt
          var continue = true
          if (nextNode.isUpper) {
            if ((keyInts(nextNode.toLower) & nextKeyInt) == 0) {
              continue = false
            }
          } else if (nextNode.isLower) {
            nextKeyInt |= keyInts(nextNode)
          }

          if (continue && smaller(newDistance, nextNode, nextKeyInt)) {
            charKeyDistances((nextNode, nextKeyInt)) = newDistance
            queue.enqueue((newDistance, nextKeyInt, nextNode))
          }
        })
      }
    }

    println("result1", minValue)
  }

  // Part 2
  val map2 = map.clone()

  // Add 4 numbered robots
  map2((startPos._1 - 1, startPos._2 - 1)) = '1'
  map2((startPos._1 + 1, startPos._2 - 1)) = '2'
  map2((startPos._1 + 1, startPos._2 + 1)) = '3'
  map2((startPos._1 - 1, startPos._2 + 1)) = '4'
  map2((startPos._1 - 1, startPos._2)) = '#'
  map2((startPos._1 + 1, startPos._2)) = '#'
  map2(startPos) = '#'
  map2((startPos._1, startPos._2 - 1)) = '#'
  map2((startPos._1, startPos._2 + 1)) = '#'

  part2()

  def part2(): Unit = {
    val newNodes: Array[Char] = Array('1', '2', '3', '4').concat(doorLookup.keys).concat(keyLookup.keys)

    val nodeLocations: mutable.Map[Char, (Int, Int)] = mutable.Map.empty
    nodeLocations('1') = (startPos._1 - 1, startPos._2 - 1)
    nodeLocations('2') = (startPos._1 + 1, startPos._2 - 1)
    nodeLocations('3') = (startPos._1 + 1, startPos._2 + 1)
    nodeLocations('4') = (startPos._1 - 1, startPos._2 + 1)
    nodeLocations.addAll(doorLookup)
    nodeLocations.addAll(keyLookup)

    val nodeDistances = mutable.Map.empty[(Char, Char), Int]
    var visitedPositions = mutable.Set.empty[(Int, Int)]
    val nodeQueue = mutable.Queue.empty[((Int, Int), Int)]

    val offsets = Array(
      (0, -1),
      (1, 0),
      (0, 1),
      (-1, 0),
    )

    for (node <- newNodes.toList.sorted) {
      visitedPositions = mutable.Set.empty[(Int, Int)]

      val startPos: (Int, Int) = nodeLocations(node)
      nodeQueue.enqueue((startPos, 0))
      visitedPositions(startPos) = true

      while (nodeQueue.nonEmpty) {
        val tuple = nodeQueue.dequeue()
        val currentPos = tuple._1
        val currentDis = tuple._2
        val currentChar = map2(currentPos)
        if (newNodes.contains(currentChar) && currentChar != node) {
          nodeDistances((node, currentChar)) = currentDis
        } else {
          offsets.foreach(offset => {
            val newPos = (currentPos._1 + offset._1, currentPos._2 + offset._2)
            if (map2(newPos) != '#') {
              if (!visitedPositions.contains(newPos)) {
                visitedPositions(newPos) = true
                nodeQueue.enqueue((newPos, currentDis + 1))
              }
            }
          })
        }
      }
    }

    val startNodes = "1234"
    val reachableKeys = mutable.Map.empty[Char, Int]
    val reachableVisited = mutable.Set.empty[Char]

    startNodes.foreach(startNode => {
      reachableVisited.add(startNode)
      reachableKeys(startNode) = 0
      val nodeQueue = mutable.Queue(startNode)
      while (nodeQueue.nonEmpty) {
        val currentNode = nodeQueue.dequeue()
        nodeDistances.filter(n => n._1._1 == currentNode).map(n => n._1._2).foreach(nxtNode => {
          if (!reachableVisited.contains(nxtNode)) {
            reachableVisited.add(nxtNode)
            if (nxtNode.isLower) {
              reachableKeys(startNode) |= keyInts(nxtNode)
            }
            nodeQueue.enqueue(nxtNode)
          }
        })
      }
    })

    val stringKeyDistances = mutable.Map.empty[(String, Int), Int]
    val distanceQueue = mutable.PriorityQueue.empty[(Int, Int, String)](
      Ordering.by((_: (Int, Int, String))._1).reverse
    )
    distanceQueue.enqueue((0, 0, startNodes))
    stringKeyDistances((startNodes, 0)) = 0

    def smaller(newDist: Int, newNodes: String, intKeyMap: Int): Boolean = {
      if (!stringKeyDistances.contains((newNodes, intKeyMap))) {
        true
      } else {
        newDist < stringKeyDistances((newNodes, intKeyMap))
      }
    }

    var min = Int.MaxValue

    while (distanceQueue.nonEmpty) {
      val current = distanceQueue.dequeue()
      val currentDistance = current._1
      val currentKeyInt = current._2
      val currentNodes = current._3

      if (currentKeyInt == keyIntsSum) {
        min = currentDistance
        distanceQueue.clear()
      } else {

        startNodes.zipWithIndex.foreach(tuple => {
          val startNode = tuple._1
          val startNodeIndex = tuple._2
          breakable {
            if ((reachableKeys(startNode) & currentKeyInt) == reachableKeys(startNode)) {
              break
            }
            val currentNode = currentNodes(startNodeIndex)
            nodeDistances.filter(n => n._1._1 == currentNode).map(n => n._1._2).toList.sorted.foreach(newNode => {
              breakable {
                val newDist = currentDistance + nodeDistances((currentNode, newNode))
                val newNodesList = currentNodes.toList.updated(startNodeIndex, newNode)
                val newNodes = newNodesList.mkString
                var newKeyMap = currentKeyInt
                if (newNode.isUpper) {
                  if ((keyInts(newNode.toLower) & currentKeyInt) == 0) {
                    break
                  }
                } else if (newNode.isLower) {
                  newKeyMap |= keyInts(newNode)
                }

                if (smaller(newDist, newNodes, newKeyMap)) {
                  stringKeyDistances((newNodes, newKeyMap)) = newDist
                  distanceQueue.enqueue((newDist, newKeyMap, newNodes))
                }
              }
            })
          }
        })
      }
    }

    println("result2", min)
  }
}
