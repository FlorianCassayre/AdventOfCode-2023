package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day03 = Day(3) { (input, part) =>

  case class Vec(i: Int, j: Int):
    def +(that: Vec) = Vec(i + that.i, j + that.j)
    def around = (-1 to 1).flatMap(i1 => (-1 to 1).map(j1 => Vec(i + i1, j + j1)))

  val grid = input.toLines

  val numbers = grid.zipWithIndex.flatMap((s, i) => s.tails.zipWithIndex.toSeq.init.zip(None +: s.init.map(Some.apply))
    .collect {
      case ((t, j), p) if p.forall(!_.isDigit) && t.head.isDigit =>
        val digits = t.takeWhile(_.isDigit)
        digits.toInt -> digits.indices.map(k => Vec(i, j + k))
    }
  )
  val symbols = grid.zipWithIndex.flatMap((s, i) => s.zipWithIndex.filter((c, j) => !c.isDigit && c != '.').map((c, j) => c -> Vec(i, j)))
  val symbolsAround = symbols.flatMap((_, p) => p.around).toSet

  part(1) = numbers.filter((_, vs) => vs.exists(symbolsAround.contains)).map((n, _) => n).sum

  val gears = symbols.filter((c, _) => c == '*').flatMap((_, p) => p.around.map(_ -> p)).toMap

  part(2) = numbers
    .flatMap((n, vs) => vs.flatMap(gears.get).toSet.map(_ -> n))
    .groupBy((p, _) => p).values.filter(_.sizeIs == 2)
    .map(_.map((_, n) => n).product).sum

}
