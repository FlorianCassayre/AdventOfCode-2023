package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day11 = Day(11) { (input, part) =>

  case class Vec(i: Int, j: Int):
    inline def -(that: Vec): Vec = Vec(i - that.i, j - that.j)
    def manhattan: Int = i.abs + j.abs

  val map = input.toLines.map(_.map {
    case '.' => false
    case '#' => true
  })

  def compute(multiplier: Int): Long =
    def indexMapper(map: IndexedSeq[IndexedSeq[Boolean]]): Map[Int, Int] =
      Seq.unfold((0, map.map(_.exists(identity)))) {
        case (i, b +: t) => Some((i, (i + (if b then 1 else multiplier), t)))
        case _ => None
      }.zipWithIndex.map(_.swap).toMap
    val (rows, columns) = (indexMapper(map), indexMapper(map.transpose))
    map.zipWithIndex.flatMap((row, i) => row.zipWithIndex.collect { case (true, j) => Vec(i, j) })
      .map(vec => Vec(rows(vec.i), columns(vec.j)))
      .tails.toSeq.init.flatMap(tail => tail.tail.map(t => (tail.head, t)))
      .map((a, b) => (a - b).manhattan.toLong).sum

  part(1) = compute(2)

  part(2) = compute(1000000)

}
