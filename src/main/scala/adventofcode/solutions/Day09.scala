package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day09 = Day(9) { (input, part) =>

  val histories = input.toLines.map(_.split(" ").map(_.toInt).toVector).toVector

  def differences(seq: Seq[Int]): Seq[Seq[Int]] =
    if seq.forall(_ == 0) then
      Seq(seq)
    else
      seq +: differences(seq.zip(seq.tail).map((a, b) => b - a))

  def extrapolate(seq: Seq[Int]): Int = differences(seq).reverse.map(_.last).sum

  part(1) = histories.map(extrapolate).sum

  def extrapolateBack(seq: Seq[Int]): Int =
    differences(seq).reverse.map(_.head).foldLeft(0)((acc, e) => e - acc)

  part(2) = histories.map(extrapolateBack).sum

}
