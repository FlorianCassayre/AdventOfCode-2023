package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day06 = Day(6) { (input, part) =>

  case class Race(time: Long, distance: Long)

  val races = input.toLines match
    case Seq(s"Time: $times", s"Distance: $distances") =>
      def parse(s: String): Seq[Long] = s.trim.split(" +").map(_.toLong)
      (parse(times).zip(parse(distances)).map((time, distance) => Race(time, distance)))

  def count(race: Race): Long =
    val sqrtDelta = Math.sqrt(race.time * race.time - 4 * race.distance)
    val (min, max) = ((race.time - sqrtDelta) / 2, (race.time + sqrtDelta) / 2)
    (min.ceil.toLong to max.floor.toLong).size

  part(1) = races.map(count).product

  val singleRace =
    def concat(numbers: Seq[Long]): Long = numbers.map(_.toString).mkString.toLong
    Race(concat(races.map(_.time)), concat(races.map(_.distance)))

  part(2) = count(singleRace)

}
