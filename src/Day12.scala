import scala.collection.mutable
import scala.io.Source

object Day12 extends App {
  val file = Source.fromFile("./input/day12")
  val lines = file.getLines.toArray
  file.close()

  class Moon(var pos: (Int, Int, Int), var velocity: (Int, Int, Int)) {
    def energy: Int = {
      val potential = Math.abs(pos._1) + Math.abs(pos._2) + Math.abs(pos._3)
      val kinetic = Math.abs(velocity._1) + Math.abs(velocity._2) + Math.abs(velocity._3)
      val energy = potential * kinetic
      energy
    }

    override def toString = s"Moon($pos, $velocity)"


    def canEqual(other: Any): Boolean = other.isInstanceOf[Moon]

    override def equals(other: Any): Boolean = other match {
      case that: Moon =>
        (that canEqual this) &&
          pos == that.pos &&
          velocity == that.velocity
      case _ => false
    }

    override def hashCode(): Int = {
      val state = Seq(pos, velocity)
      state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
  }

  private def ParseMoons: List[Moon] = {
    lines.map((line: String) => {
      val regex = """<x=\s*(-?\d+), y=\s*(-?\d+), z=\s*(-?\d+)>""".r
      line match {
        case regex(x, y, z) => new Moon(
          (x.toInt, y.toInt, z.toInt),
          (0, 0, 0))
      }
    }).toList
  }

  val moons1: List[Day12.Moon] = ParseMoons
  //  println(moons1)

  private def step(moons: Seq[Moon]): Unit = {
    // Update Velocity
    var tmpMoons = moons.toArray
    while (tmpMoons.length > 1) {
      val m1 = tmpMoons.head
      tmpMoons.tail.foreach((m2: Moon) => {
        def getdiff(p1: Int, p2: Int): (Int, Int) = {
          if (p1 < p2) {
            (1, -1)
          } else if (p1 == p2) {
            (0, 0)
          } else {
            (-1, 1)
          }
        }

        val diffX = getdiff(m1.pos._1, m2.pos._1)
        val diffY = getdiff(m1.pos._2, m2.pos._2)
        val diffZ = getdiff(m1.pos._3, m2.pos._3)

        m1.velocity = (
          m1.velocity._1 + diffX._1,
          m1.velocity._2 + diffY._1,
          m1.velocity._3 + diffZ._1
        )
        m2.velocity = (
          m2.velocity._1 + diffX._2,
          m2.velocity._2 + diffY._2,
          m2.velocity._3 + diffZ._2
        )
      })
      tmpMoons = tmpMoons.tail
    }

    // Step
    moons.foreach(moon => {
      moon.pos = (
        moon.pos._1 + moon.velocity._1,
        moon.pos._2 + moon.velocity._2,
        moon.pos._3 + moon.velocity._3
      )
    })
  }


  for (_ <- 1 to 1000) {
    step(moons1)
  }

  private val sum: Int = moons1.map((moon: Moon) => {
    moon.energy
  }).sum

  println(sum)

  val moons2: List[Day12.Moon] = ParseMoons
  if (moons2.length != 4) {
    throw new Error()
  }

  def getx: (Int, Int, Int, Int, Int, Int, Int, Int) = {
    (
      moons2.head.pos._1,
      moons2(1).pos._1,
      moons2(2).pos._1,
      moons2(3).pos._1,
      moons2.head.velocity._1,
      moons2(1).velocity._1,
      moons2(2).velocity._1,
      moons2(3).velocity._1,
    )
  }

  def gety: (Int, Int, Int, Int, Int, Int, Int, Int) = {
    (
      moons2.head.pos._2,
      moons2(1).pos._2,
      moons2(2).pos._2,
      moons2(3).pos._2,
      moons2.head.velocity._2,
      moons2(1).velocity._2,
      moons2(2).velocity._2,
      moons2(3).velocity._2,
    )
  }

  def getz: (Int, Int, Int, Int, Int, Int, Int, Int) = {
    (
      moons2.head.pos._3,
      moons2(1).pos._3,
      moons2(2).pos._3,
      moons2(3).pos._3,
      moons2.head.velocity._3,
      moons2(1).velocity._3,
      moons2(2).velocity._3,
      moons2(3).velocity._3
    )
  }

  private val xSet: mutable.Set[(Int, Int, Int, Int, Int, Int, Int, Int)] = mutable.Set(getx)
  private val ySet: mutable.Set[(Int, Int, Int, Int, Int, Int, Int, Int)] = mutable.Set(gety)
  private val zSet: mutable.Set[(Int, Int, Int, Int, Int, Int, Int, Int)] = mutable.Set(getz)

  var rep = (0, 0, 0)
  var count = 0
  while (rep._1 == 0 || rep._2 == 0 || rep._3 == 0) {
    step(moons2)
    count += 1

    if (rep._1 == 0) {
      val x = getx
      if (xSet.contains(x)) {
        rep = (count, rep._2, rep._3)
      }
    }

    if (rep._2 == 0) {
      val y = gety
      if (ySet.contains(y)) {
        rep = (rep._1, count, rep._3)
      }
    }

    if (rep._3 == 0) {
      val z = getz
      if (zSet.contains(z)) {
        rep = (rep._1, rep._2, count)
      }
    }
  }

  //  println(rep)

  @scala.annotation.tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

  def lcm(a: Long, b: Long, c: Long): Long = lcm(lcm(a, b), c)

  val circle = lcm(rep._1, rep._2, rep._3)
  println(circle)
}
