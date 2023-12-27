package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day21 = Day(21) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)

  val neighbours =
    val range = -1 to 1
    for
      i <- range
      j <- range
      if (i == 0) != (j == 0)
    yield Vec(i, j)

  val mapWithStart = input.toLines.map(_.map {
    case '#' => Some(true)
    case '.' => Some(false)
    case 'S' => None
  }.toIndexedSeq)
  val map = mapWithStart.map(_.map(_.getOrElse(false)))
  val start = mapWithStart.view.zipWithIndex.flatMap((row, i) => row.view.zipWithIndex.collect { case (None, j) => Vec(i, j) }).head

  def mod(n: Int, m: Int): Int = ((n % m) + m) % m

  def reachable(i: Int, positions: Set[Vec] = Set(start)): Int =
    if i > 0 then
      val nextPositions = positions
        .flatMap(p => neighbours.map(p + _))
        .filter(p => !map(mod(p.i, map.size))(mod(p.j, map.head.size)))
      reachable(i - 1, nextPositions)
    else
      positions.size

  val v0 = reachable(map.size / 2 - 1)

  part(1) = v0

  val (v1, v2) = (reachable(map.size / 2), reachable(map.size * 3 / 2))
  val (d0, d1) = (v1 - v0, v2 - v1)
  val dd0 = d1 - d0

  val (a, c) = (dd0 / 2, v0)
  val b = a + c - v1

  def f(n: Long): Long = a * n * n - b * n + c

  val magic = 26501365

  part(2) = f(magic / map.size + 1)

}
