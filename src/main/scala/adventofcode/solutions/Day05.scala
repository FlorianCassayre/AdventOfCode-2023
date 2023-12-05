package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.immutable.NumericRange

@main def Day05 = Day(5) { (input, part) =>
  case class Almanac(seeds: Seq[Long], maps: Seq[Seq[Mapping]]):
    def seedRanges: Seq[Range] = seeds.grouped(2).map { case Seq(start, length) => Range(start, length) }.toSeq
  case class Mapping(destinationStart: Long, sourceStart: Long, length: Long):
    def convert(seed: Long): Option[Long] =
      if (sourceStart until sourceStart + length).contains(seed) then Some(seed - sourceStart + destinationStart) else None
    def invert(seed: Long): Option[Long] =
      if (destinationStart until destinationStart + length).contains(seed) then Some(seed - destinationStart + sourceStart) else None
  case class Range(start: Long, length: Long):
    def end: Long = start + length
    def intersect(that: Range): Range = Range(start.max(that.start), end.min(that.end))
    def range = start until start + length

  val almanac = input.toLines match
    case s"seeds: $seeds" +: _ +: rest =>
      val maps = rest.mkString(lineSeparator).split(lineSeparator * 2).map(_.split(lineSeparator).toSeq match
        case s"$_-to-$_ map:" +: triplets =>
          triplets.map {
            case s"$destinationStart $sourceStart $length" => Mapping(destinationStart.toLong, sourceStart.toLong, length.toLong)
          }
      )
      Almanac(seeds.split(" ").map(_.toLong), maps)

  def convert(seeds: Seq[Long], map: Seq[Mapping]): Seq[Long] =
    seeds.map(seed => map.view.flatMap(_.convert(seed)).headOption.getOrElse(seed))

  part(1) = almanac.maps.foldLeft(almanac.seeds)(convert).min

  def invertStep(map: Seq[Mapping], seed: Long): Long = map.view.flatMap(_.invert(seed)).headOption.getOrElse(seed)
  val resultIntervals = almanac.seedRanges

  def invert(n: Long): Long = almanac.maps.foldRight(n)(invertStep)

  val maxPoint = almanac.seedRanges.map(_.end).max

  def walk(n: Long, step: Long, points: Seq[Long]): Seq[Long] =
    val (v0, v1, v2) = (invert(n - 1), invert(n), invert(n + step))
    val expected = v1 + (v1 - v0) * step
    if n <= maxPoint then
      if v2 == expected then
        walk(n + step, step * 2, points)
      else
        if step == 1 then walk(n + 1, 1, (n + 1) +: points) else walk(n, step / 2, points)
    else
      points

  part(2) = walk(0, 1, Seq.empty).sliding(2).flatMap {
    case Seq(start, end) =>
      val startValue = invert(start)
      val range = Range(startValue, end - start)
      resultIntervals.map(_.intersect(range)).filter(_.length > 0).map(interval => (interval.start - startValue) + start)
  }.filter(m => resultIntervals.exists(_.range.contains(invert(m)))).min

}
