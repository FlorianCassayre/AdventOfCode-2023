package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day12 = Day(12) { (input, part) =>

  case class Row(row: IndexedSeq[Option[Boolean]], runLength: Seq[Int])

  val rows = input.toLines.map {
    case s"$lhs $rhs" =>
      val row = lhs.map {
        case '.' => Some(false)
        case '#' => Some(true)
        case '?' => None
      }
      Row(row, rhs.split(",").map(_.toInt).toSeq)
  }

  type Cache = Map[(Int, Int, Int), Long]

  def arrangements(row: Row): Long =
    def arrangements(patterns: Seq[Option[Boolean]], items: Seq[Int], patternsOffset: Int, itemsOffset: Int, slack: Int, cache: Cache): (Long, Cache) =
      val key = (patternsOffset, itemsOffset, slack)
      cache.get(key) match
        case Some(value) => (value, cache)
        case _ =>
          val (result, newCache) = items match
            case head +: tail =>
              (0 to slack)
                .filter(i => patterns.take(i).forall(!_.exists(identity))
                  && patterns.drop(i).take(head).forall(_.forall(identity))
                  && !patterns.drop(i + head).headOption.flatten.exists(identity)
                )
                .foldLeft((0L, cache)) { case ((sum, cache), i) =>
                  val offset = i + head + 1
                  val (add, newCache) = arrangements(patterns.drop(offset), tail, patternsOffset - offset, itemsOffset - 1, slack - i, cache)
                  (sum + add, newCache)
                }
            case _ => (if patterns.forall(!_.exists(identity)) then 1L else 0L, cache)
          (result, newCache + (key -> result))
    arrangements(row.row, row.runLength, 0, 0, row.row.size - (row.runLength.sum + row.runLength.size - 1), Map.empty)._1

  def sum(rows: Seq[Row]): Long = rows.map(arrangements).sum

  part(1) = sum(rows)

  def unfold(row: Row, n: Int): Row =
    Row(Seq.fill(n)(row.row).reduce(_ ++ Seq(None) ++ _), Seq.fill(n)(row.runLength).flatten)

  part(2) = sum(rows.map(unfold(_, 5)))

}
