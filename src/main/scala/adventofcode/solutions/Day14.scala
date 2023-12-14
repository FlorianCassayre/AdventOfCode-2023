package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day14 = Day(14) { (input, part) =>

  case class Vec(i: Int, j: Int)

  val mapWithRocks = input.toLines.map(_.map {
    case '.' => None
    case '#' => Some(true)
    case 'O' => Some(false)
  }.toIndexedSeq)

  val map = mapWithRocks.map(_.map {
    case Some(true) => true
    case _ => false
  })

  val initialRocks = mapWithRocks.zipWithIndex.flatMap((row, i) => row.zipWithIndex.collect {
    case (Some(false), j) => Vec(i, j)
  }).toSet

  def slide(map: IndexedSeq[IndexedSeq[Boolean]], rocks: Set[Vec]): Set[Vec] =
    rocks.toSeq.sortBy(_.i).foldLeft(Set.empty[Vec])((rocks, rock) => {
      rocks + (0 to rock.i).reverse.view.map(i => Vec(i, rock.j)).takeWhile(next => !map(next.i)(next.j) && !rocks.contains(next)).last
    })

  def rotate(map: IndexedSeq[IndexedSeq[Boolean]], rocks: Set[Vec]): (IndexedSeq[IndexedSeq[Boolean]], Set[Vec]) =
    val newMap = map.transpose.map(_.reverse)
    val newSet = rocks.map(rock => Vec(rock.j, map.size - 1 - rock.i))
    (newMap, newSet)

  def revolution(rocks: Set[Vec]): Set[Vec] =
    (0 until 4).foldLeft((map, rocks)) { case ((map, rocks), _) =>
      val slided = slide(map, rocks)
      rotate(map, slided)
    }._2

  def load(rocks: Set[Vec]): Int = rocks.toSeq.map(rock => map.size - rock.i).sum

  part(1) = load(slide(map, initialRocks))

  def iterate(rocks: Set[Vec], history: Map[Set[Vec], Int], i: Int): Int =
    if i > 0 then
      iterate(
        revolution(rocks),
        history + (rocks -> i),
        history.get(rocks).map(_ - i).map(i % _).getOrElse(i) - 1
      )
    else
      load(rocks)

  part(2) = iterate(initialRocks, Map.empty, 1000000000)

}
