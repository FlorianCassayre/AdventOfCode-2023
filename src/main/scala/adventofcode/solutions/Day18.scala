package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day18 = Day(18) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    infix def *(v: Int): Vec = Vec(i * v, j * v)
    def cw: Vec = Vec(j, -i)
    def ccw: Vec = Vec(-j, i)

  val directionsMapping = IndexedSeq(
    "R" -> Vec(0, 1),
    "D" -> Vec(1, 0),
    "L" -> Vec(0, -1),
    "U" -> Vec(-1, 0)
  )
  val directionsMap = directionsMapping.toMap
  val directions = directionsMapping.map((_, d) => d)

  case class Instruction(direction: Vec, steps: Int, color: String):
    def fromColor: Instruction =
      copy(direction = directions(color.last.asDigit), steps = Integer.parseInt(color.init, 16))

  val instructions = input.toLines.map {
    case s"$r $n (#$c)" =>
      Instruction(directionsMap(r), n.toInt, c)
  }

  def area(instructions: Seq[Instruction]): Long =
    val start = Vec(0, 0)
    def area(normal: Vec => Vec): Long =
      val positions = instructions.scanLeft(start)((position, instruction) => position + instruction.direction * instruction.steps)
      val normals = instructions.map(_.direction).map(normal)
      def floor(v: Int): Int = if v > 0 then v else 0
      val offset = normals.zip(normals.tail :+ normals.head).map(_ + _).map(v => Vec(floor(v.i), floor(v.j)))
      val border = positions.zip(offset.last +: offset.init).map(_ + _)
      border.zip(border.tail).map((a, b) => a.i.toLong * b.j - b.i.toLong * a.j).sum.abs / 2
    area(_.cw).max(area(_.ccw))

  part(1) = area(instructions)

  part(2) = area(instructions.map(_.fromColor))

}
