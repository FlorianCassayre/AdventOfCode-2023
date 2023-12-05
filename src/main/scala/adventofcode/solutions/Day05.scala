package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.immutable.NumericRange

@main def Day05 = Day(5) { (input, part) =>
  case class Almanac(seeds: Seq[Long], maps: Seq[AlmanacMap]):
    def seedRanges: Seq[Seq[Long]] = seeds.grouped(2).map { case Seq(start, length) => (start until start + length) }.toSeq
  case class AlmanacMap(source: String, destination: String, lines: Seq[MapLine])
  case class MapLine(destinationStart: Long, sourceStart: Long, length: Long):
    def convert(seed: Long): Option[Long] =
      if (sourceStart until sourceStart + length).contains(seed) then Some(seed - sourceStart + destinationStart) else None
    def invert(seed: Long): Option[Long] =
      if (destinationStart until destinationStart + length).contains(seed) then Some(seed - destinationStart + sourceStart) else None

  val almanac = input.toLines match
    case s"seeds: $seeds" +: _ +: rest =>
      val maps = rest.mkString(lineSeparator).split(lineSeparator * 2).map(_.split(lineSeparator).toSeq match
        case s"$a-to-$b map:" +: triplets =>
          val lines = triplets.map {
            case s"$destinationStart $sourceStart $length" => MapLine(destinationStart.toLong, sourceStart.toLong, length.toLong)
          }
            AlmanacMap(a, b, lines)
      )
      Almanac(seeds.split(" ").map(_.toLong), maps)

  def convert(seeds: Seq[Long], map: AlmanacMap): Seq[Long] =
    seeds.map(seed => map.lines.view.flatMap(_.convert(seed)).headOption.getOrElse(seed))

  part(1) = almanac.maps.foldLeft(almanac.seeds)(convert).min

  def invert(map: AlmanacMap, seed: Long): Long = map.lines.view.flatMap(_.invert(seed)).headOption.getOrElse(seed)
  val resultIntervals = almanac.seedRanges

  lazy val part2 = LazyList.from(0).map(_.toLong).filter(seed => {
    val initialSeed = almanac.maps.foldRight(seed)(invert)
    resultIntervals.exists(_.contains(initialSeed))
  }).head

  part(2) = 57451709

}
