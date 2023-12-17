package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day16 = Day(16) { (input, part) =>

  case class Vec(i: Int, j: Int):
    inline def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    inline def *(v: Int): Vec = Vec(i * v, j * v)
    def dot(that: Vec): Int = i * that.i + j * that.j
  case class Beam(position: Vec, direction: Vec):
    def moved: Beam = copy(position = position + direction)

  enum Tile:
    case Empty
    case Mirror(nesw: Boolean)
    case Splitter(vertical: Boolean)
  import Tile.*

  val map = input.toLines.map(_.map {
    case '.' => Empty
    case '/' => Mirror(true)
    case '\\' => Mirror(false)
    case '|' => Splitter(true)
    case '-' => Splitter(false)
  })

  val Zero = Vec(0, 0)
  val initialBeam = Beam(Zero, Vec(0, 1))

  def move(beam: Beam, tile: Tile): Set[Beam] =
    val newBeams = tile match
      case Empty => Set(beam)
      case Mirror(nesw) =>
        val normal = if nesw then Vec(1, 1) else Vec(-1, 1)
        val absNormal = if normal.dot(beam.direction) > 0 then normal else normal * -1
        val newDirection = beam.direction + absNormal * -1 * beam.direction.dot(absNormal)
        Set(beam.copy(direction = newDirection))
      case Splitter(vertical) =>
        val direction = if vertical then Vec(1, 0) else Vec(0, 1)
        if beam.direction.dot(direction) == 0 then
          Set(beam.copy(direction = direction), beam.copy(direction = direction * -1))
        else
          Set(beam)
    newBeams.map(_.moved)

  def simulate(beams: Set[Beam], history: Set[Beam]): Int =
    if beams.nonEmpty then
      val newBeams = beams.flatMap(beam => move(beam, map(beam.position.i)(beam.position.j)))
        .filter(beam => map.indices.contains(beam.position.i) && map(beam.position.i).indices.contains(beam.position.j))
        .diff(history)
      val newHistory = history ++ beams
      simulate(newBeams, newHistory)
    else
      history.map(_.position).size

  part(1) = simulate(Set(initialBeam), Set.empty)

  val borders =
    Seq(
      (map.head.indices.map(Vec(0, _)), Vec(1, 0)),
      (map.indices.map(Vec(_, 0)), Vec(0, 1)),
      (map.indices.map(Vec(_, map.head.size - 1)), Vec(0, -1)),
      (map.head.indices.map(Vec(map.size - 1, _)), Vec(-1, 0)),
    ).flatMap((positions, direction) => positions.map(position => Beam(position, direction)))

  part(2) = borders.map(beam => simulate(Set(beam), Set.empty)).max

}
