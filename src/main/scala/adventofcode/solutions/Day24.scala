package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day24 = Day(24) { (input, part) =>

  case class Rational(numerator: BigInt, denominator: BigInt):
    def abs: Rational = Rational(numerator.abs, denominator)
    infix def +(that: Rational): Rational = Rational(numerator * that.denominator + that.numerator * denominator, denominator * that.denominator)
    infix def -(that: Rational): Rational = Rational(numerator * that.denominator - that.numerator * denominator, denominator * that.denominator)
    infix def unary_- : Rational = Rational(-numerator, denominator)
    infix def *(that: Rational): Rational = Rational(numerator * that.numerator, denominator * that.denominator)
    infix def /(that: Rational): Rational = Rational(numerator * that.denominator, denominator * that.numerator)
  object Rational:
    def apply(n: BigInt, d: BigInt = BigInt(1)): Rational =
      def gcd(a: BigInt, b: BigInt): BigInt = if b == BigInt(0) then a.abs else gcd(b, a % b)
      val m = gcd(n, d)
      new Rational((if n * d >= 0 then 1 else -1) * n.abs / m, d.abs / m)
  given rationalOrdering: Ordering[Rational] = (a, b) => {
    val sign = a.numerator * b.denominator - a.denominator * b.numerator
    if sign > 0 then 1 else if sign < 0 then -1 else 0
  }
  val Zero = Rational(BigInt(0L))
  val One = Rational(BigInt(1L))

  case class Vec(x: Rational, y: Rational, z: Rational):
    infix def +(that: Vec): Vec = Vec(x + that.x, y + that.y, z + that.z)
    infix def -(that: Vec): Vec = Vec(x - that.x, y - that.y, z - that.z)
    infix def *(v: Rational): Vec = Vec(x * v, y * v, z * v)
    def toIndexedSeq: IndexedSeq[Rational] = IndexedSeq(x, y, z)

  case class Stone(position: Vec, velocity: Vec):
    def apply(v: Rational): Vec = position + velocity * v

  def gaussianElimination(matrix: IndexedSeq[IndexedSeq[Rational]]): IndexedSeq[IndexedSeq[Rational]] = {
    val (m, n) = (matrix.size, matrix.head.size)
    val (reducedMatrix, _) = (0 until m).foldLeft((matrix, -1)) { case ((matrix1, r), j) =>
      val k = (r + 1 until m).maxBy(i => matrix1(i)(j).abs)
      val pivot = matrix1(k)(j)
      if (pivot.abs != Zero) {
        val r1 = r + 1
        val matrix2 = (0 until n).foldLeft(matrix1)((current, l) => current.updated(k, current(k).updated(l, current(k)(l) / pivot)))
        val matrix3 = (0 until n).foldLeft(matrix2)((current, l) => current.updated(k, current(k).updated(l, current(r1)(l))).updated(r1, current(r1).updated(l, current(k)(l))))
        val matrix4 = (0 until m).foldLeft(matrix3) { (current, i) =>
          val v = current(i)(j)
          if (i != r1) {
            (0 until n).foldLeft(current)((current, l) => current.updated(i, current(i).updated(l, current(i)(l) - current(r1)(l) * v)))
          } else {
            current
          }
        }
        (matrix4, r1)
      } else {
        (matrix1, r)
      }
    }
    reducedMatrix
  }

  val stones = input.toLines.map {
    case s"$position @ $velocity" =>
      def parseVec(s: String): Vec = s match
        case s"$x, $y, $z" => Vec(Rational(x.trim.toLong), Rational(y.trim.toLong), Rational(z.trim.toLong))
      Stone(parseVec(position), parseVec(velocity))
  }

  def intersectionXY(a: Stone, b: Stone): Option[(Rational, Rational)] =
    val matrix = IndexedSeq(
      a.velocity.toIndexedSeq,
      b.velocity.toIndexedSeq.map(-_),
      b.position.toIndexedSeq.zip(a.position.toIndexedSeq).map((a, b) => a - b)
    ).transpose.init
    val result = gaussianElimination(matrix)
    if result(1)(1) == One then Some((result(0)(2), result(1)(2))) else None

  def inXY(vec: Vec): Boolean =
    def inAxis(v: Rational): Boolean =
      val (min, max) = (Rational(200000000000000L), Rational(400000000000000L))
      rationalOrdering.gteq(v, min) && rationalOrdering.lteq(v, max)
    inAxis(vec.x) && inAxis(vec.y)

  part(1) = stones.indices.flatMap(i => stones.indices.drop(i + 1).map(j => stones(i) -> stones(j)))
    .count((a, b) => intersectionXY(a, b)
      .filter((s, t) => s.numerator >= 0 && t.numerator >= 0)
      .exists((s, t) => Seq(a(s), b(t)).forall(inXY))
    )

  // (third party tool)

  part(2) = 983620716335751L

}
