package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day01 = Day(1) { (input, part) =>

  def sum(table: String => Option[Int]) = input.toLines.map(_.tails.flatMap(table).toSeq).map(r => s"${r.head}${r.last}".toInt).sum

  def asDigit(s: String) = s.headOption.filter(_.isDigit).map(_.asDigit)

  part(1) = sum(asDigit)

  val digits = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  part(2) = sum(s => asDigit(s).orElse(Some(digits.indexWhere(s.startsWith) + 1).filter(_ > 0)))

}
