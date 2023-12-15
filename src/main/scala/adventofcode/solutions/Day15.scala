package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day15 = Day(15) { (input, part) =>

  val strings = input.split(",")

  val range = 256
  def hash(s: String): Int = s.foldLeft(0)((v, c) => ((v + c) * 17) % range)

  part(1) = strings.map(hash).sum

  val operations = strings.map {
    case s"$s-" => (s, None)
    case s"$s=$f" => (s, Some(f.toInt))
  }

  val boxes = operations.foldLeft(IndexedSeq.fill(range)(Seq.empty[(String, Int)])) { case (boxes, (s, e)) =>
    val h = hash(s)
    val content = boxes(h)
    val insert = e.map((s, _))
    val newContent = content.zipWithIndex.view.collectFirst { case ((`s`, _), f) => f } match
      case Some(f) =>
        val (left, right) = content.splitAt(f)
        left ++ insert ++ right.tail
      case None => content ++ insert
    boxes.updated(h, newContent)
  }

  def power(boxes: IndexedSeq[Seq[(String, Int)]]): Int =
    boxes.zipWithIndex.flatMap((seq, i) => seq.zipWithIndex.map { case ((_, v), j) => (i + 1) * (j + 1) * v }).sum

  part(2) = power(boxes)

}
