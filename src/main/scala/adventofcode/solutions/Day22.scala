package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day22 = Day(22) { (input, part) =>

  case class Vec(x: Int, y: Int, z: Int):
    infix def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)
    infix def >>=(that: Vec): Boolean = x >= that.x && y >= that.y && z >= that.z

  case class Brick(a: Vec, b: Vec):
    def min: Vec = Vec(a.x.min(b.x), a.y.min(b.y), a.z.min(b.z))
    def max: Vec = Vec(a.x.max(b.x), a.y.max(b.y), a.z.max(b.z))
    def intersect(that: Brick): Boolean =
      val (minA, maxA) = (min, max)
      val (minB, maxB) = (that.min, that.max)
      (maxA >>= minB) && (maxB >>= minA)
    def xyNoIntersection(that: Brick): Boolean =
      val (minA, maxA) = (min, max)
      val (minB, maxB) = (that.min, that.max)
      minA.x > maxB.x || minB.x > maxA.x || minA.y > maxB.y || minB.y > maxA.y
    def lowestDz(that: Brick): Option[Int] =
      if xyNoIntersection(that) then
        None
      else
        val (aMinZ, aMaxZ) = (a.z.min(b.z), a.z.max(b.z))
        val (bMinZ, bMaxZ) = (that.a.z.min(that.b.z), that.a.z.max(that.b.z))
        if aMaxZ < bMinZ then
          None
        else
          assert(aMinZ > bMaxZ)
          val newAMinZ = bMaxZ + 1
          val steps = aMinZ - newAMinZ
          assert(steps >= 0)
          if aMaxZ - steps < bMinZ then
            None
          else
            Some(steps)

  val bricks = input.toLines.map {
    case s"$lhs~$rhs" =>
      def parseVec(s: String): Vec = s match
        case s"$x,$y,$z" => Vec(x.toInt, y.toInt, z.toInt)
      Brick(parseVec(lhs), parseVec(rhs))
  }.sortBy(_.min.z)

  def fallVec(vec: Vec): Vec = vec.copy(z = vec.z - 1)
  def fall(brick: Brick): Brick = Brick(fallVec(brick.a), fallVec(brick.b))
  def fallFast(brick: Brick, other: IndexedSeq[Brick]): Option[Brick] =
    val dz = other.flatMap(brick.lowestDz).minOption.getOrElse(brick.min.z - 1)
    if dz > 0 then
      Some(Brick(brick.a.copy(z = brick.a.z - dz), brick.b.copy(z = brick.b.z - dz)))
    else
      None

  def simulate(bricks: IndexedSeq[Brick]): IndexedSeq[Brick] =
    val sorted = bricks.sortBy(_.min.z)
    sorted.indices.foldLeft(sorted) { (bricks, i) =>
      val brick = bricks(i)
      val newBrick = fallFast(brick, bricks.take(i) ++ bricks.drop(i + 1)).getOrElse(brick)
      bricks.updated(i, newBrick)
    }

  val simulated = simulate(bricks)

  def supports(bottom: Brick, top: Brick): Boolean = fall(top).intersect(bottom)
  def createMap(f: (Brick, Brick) => Boolean): Map[Brick, Seq[Brick]] =
    simulated.indices.map { i =>
      val brick = simulated(i)
      brick -> simulated.indices.filter(_ != i).map(simulated).filter(f(brick, _))
    }.toMap

  val supportMap = createMap(supports)
  val isSupportedMap = createMap((bottom, top) => supports(top, bottom))

  part(1) = supportMap.values.count(_.forall(brick => isSupportedMap(brick).sizeIs > 1))

  def bfs(current: Set[Brick], history: Set[Brick]): Int =
    if current.nonEmpty then
      val newHistory = history ++ current
      val newCurrent = current
        .flatMap(brick => supportMap(brick))
        .filter(brick => isSupportedMap(brick).forall(other => newHistory.contains(other)))
      bfs(newCurrent, newHistory)
    else
      history.size

  part(2) = simulated.map(brick => bfs(Set(brick), Set.empty) - 1).sum

}
