package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day08 = Day(8) { (input, part) =>

  def gcd(a: Long, b: Long): Long = if b == 0 then a.abs else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)

  case class Node(left: String, right: String):
    def apply(direction: Boolean): String = if direction then right else left

  val (instructions, nodes) = input.split(lineSeparator * 2).toSeq match
    case Seq(head, tail) =>
      val instructions = head.map {
        case 'L' => false
        case 'R' => true
      }
      val nodes = tail.split(lineSeparator).map {
        case s"$start = ($left, $right)" => start -> Node(left, right)
      }.toMap
      (instructions, nodes)

  val Start = "AAA"
  val End = "ZZZ"

  def find(current: String, steps: Int): Int = current match
    case End => steps
    case _ => find(nodes(current)(instructions(steps % instructions.size)), steps + 1)

  part(1) = find(Start, 0)

  def isStart(s: String): Boolean = s.endsWith("A")

  def findCycle(current: String, seen: Map[(String, Int), Int], steps: Int): Int =
    val i = steps % instructions.size
    val key = (current, i)
    if seen.contains(key) then
      steps - seen(key)
    else
      findCycle(nodes(current)(instructions(i)), seen + (key -> steps), steps + 1)

  part(2) = nodes.keySet.filter(isStart).toSeq.map(findCycle(_, Map.empty, 0).toLong).reduce(lcm)

}
