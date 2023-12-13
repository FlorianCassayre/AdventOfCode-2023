package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.View

@main def Day13 = Day(13) { (input, part) =>

  type Pattern = IndexedSeq[IndexedSeq[Boolean]]

  val patterns = input.split(lineSeparator * 2).map(_.split(lineSeparator).map(_.map {
    case '.' => false
    case '#' => true
  }).toIndexedSeq).toIndexedSeq

  def reflection(pattern: Pattern): (Seq[Int], Seq[Int]) =
    def findReflection(pattern: Pattern): Seq[Int] =
      def reflects(pattern: Pattern, i: Int): Boolean =
        val (left, right) = pattern.splitAt(i)
        left.reverse.zip(right).forall(_ == _)
      pattern.indices.tail.filter(reflects(pattern, _))
    (findReflection(pattern), findReflection(pattern.transpose))

  def evaluate(reflection: (Seq[Int], Seq[Int])): Int =
    val (l, r) = reflection
    l.map(_ * 100).sum + r.sum

  val reflections = patterns.map(reflection)

  part(1) = reflections.map(evaluate).sum

  def smudged(pattern: Pattern): View[Pattern] =
    pattern.indices.view.flatMap(i => pattern(i).indices.view.map(j => (i, j)))
      .map((i, j) => pattern.updated(i, pattern(i).updated(j, !pattern(i)(j))))

  part(2) = patterns.zip(reflections).flatMap { case (p, (l, r)) =>
    smudged(p).map(reflection).map((l1, r1) => (l1.diff(l), r1.diff(r)))
      .find((a, b) => a.nonEmpty || b.nonEmpty)
  }.map(evaluate).sum

}
