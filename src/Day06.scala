import scala.io.Source
import scala.collection.mutable

object Day06 extends App {
  class Node(val value: String, var orbiters: List[Node], var orbits: List[Node]) {
    def countOrbiter: Int = {
      val sum = orbiters.map(orbiter => 1 + orbiter.countOrbiter).sum
      sum
    }
  }

  val file = Source.fromFile("./input/day06")
  val lines = file.getLines.toArray
  file.close()

  val orbits: Array[Array[String]] = lines.map((line: String) => line.split(')'))
  val objects: Set[String] = Set.from(orbits.flatMap(_.toList))
  val nodes = mutable.Map[String, Node]()
  objects.foreach((obj: String) => {
    nodes(obj) = new Node(obj, List[Node](), List[Node]())
  })

  nodes.values.foreach((node: Node) => {
    for (orbit <- orbits.filter(orbit => orbit(0) == node.value).map(orbit => orbit(1))) {
      val orbitNode = nodes(orbit)
      node.orbiters = node.orbiters.appended(orbitNode)
    }
  })

  var count = 0
  nodes.values.foreach((node: Node) => {
    var ncount = node.countOrbiter
    count += ncount
  })
  println(count)

  // Part 2
  nodes.values.foreach((node: Node) => {
    node.orbiters.foreach((orbiter: Node) => {
      orbiter.orbits = orbiter.orbits.appended(node)
    })
  })

  def findMinDistance(start: Node, end: Node): Int = {
    val visited = mutable.Map[String, Int]()
    visited(start.value) = 0
    var nextOtherNodes: Seq[Node] = start.orbits.concat(start.orbiters)
    var dist = 0
    while (nextOtherNodes.nonEmpty) {
      val others = nextOtherNodes.toArray
      nextOtherNodes = Array.empty[Node].toSeq
      others.foreach((other: Node) => {
        val cont = visited.get(other.value) match {
          case None =>
            true
          case prevDistance: Some[Int] =>
            prevDistance.value > dist
        }

        if (cont) {
          visited(other.value) = dist
          nextOtherNodes = nextOtherNodes.appendedAll(other.orbits.concat(other.orbiters))
        }
      })

      dist += 1
    }

    val distend = visited(end.value) - 1
    distend
  }

  var minJumps = findMinDistance(nodes("YOU"), nodes("SAN"))
  println(minJumps)
}