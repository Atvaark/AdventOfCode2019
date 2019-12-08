import scala.io.Source

object Day08 extends App {
  val file = Source.fromFile("./input/day08")
  val input = file.mkString.trim
  file.close()

  val width = 25
  val height = 6

  private val len = width * height
  private val layerCount: Int = input.length / len
  private val layers: IndexedSeq[IndexedSeq[String]] = (0 until layerCount)
    .map(i => input.substring(len * i, (len * i) + len))
    .map(layer => {
      (0 until height).map((j: Int) => layer.substring(width * j, (width * j) + width))
    })

  private val layerCounts: IndexedSeq[(IndexedSeq[String], Int)] = layers.map((layer: IndexedSeq[String]) => {
    var count = 0
    layer.foreach((row: String) => {
      count += row.count(c => c == '0')
    })

    (layer, count)
  })

  private val layerCountsSorted: IndexedSeq[(IndexedSeq[String], Int)] = layerCounts.sortBy((tuple: (IndexedSeq[String], Int)) => tuple._2)
  val minZeroDigitsLayer = layerCountsSorted.head._1
  var count1 = minZeroDigitsLayer.map((row: String) => row.count(c => c == '1')).sum
  var count2 = minZeroDigitsLayer.map((row: String) => row.count(c => c == '2')).sum
  var result = count1 * count2
  println(result)

  (0 until height).foreach((y: Int) => {
    (0 until width).foreach((x: Int) => {
      var pixel = '2'
      layers.view.reverse.foreach((layer: IndexedSeq[String]) => {
        val row = layer(y)
        val c: Char = row(x)
        c match {
          case '0' =>
            pixel = c
          case '1' =>
            pixel = c
          case _ =>
        }
      })
      print(pixel)
    })
    println()
  })
  println()
}