package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day17 = Day(17) { (input, part) =>

  case class Vec(i: Int, j: Int):
    infix def +(that: Vec): Vec = Vec(i + that.i, j + that.j)
    def cw: Vec = Vec(j, -i)

    def ccw: Vec = Vec(-j, i)

  case class State(position: Vec, direction: Vec, history: Int)

  def shortestPathDijkstra[U, N](adjacency: U => Set[(U, N)], sources: Set[U], earlyStopping: U => Boolean)(implicit ev: Numeric[N]): (Map[U, N], Map[U, U]) =
    import ev._
    import scala.collection.immutable.TreeSet
    def search(queue: TreeSet[(U, (N, Int))], distances: Map[U, N], predecessors: Map[U, U], visited: Set[U], k: Int): (Map[U, N], Map[U, U]) =
      queue.headOption match
        case Some((u, (du, _))) =>
          if earlyStopping(u) then
            (distances, predecessors)
          else if !visited.contains(u) then
            val newVisited = visited + u
            val edges = adjacency(u).filter { case (b, w) => !visited.contains(b) && (!distances.contains(b) || distances(b) > du + w) }
            val (newQueue, newDistances, newPredecessors, newK) = edges.foldLeft((queue.tail, distances, predecessors, k)) { case ((currentQueue, currentDistances, currentPredecessors, currentK), (v, w)) =>
              val newDistance = du + w
              (currentQueue + (v -> (newDistance, currentK)),
                currentDistances - v + (v -> newDistance),
                currentPredecessors + (v -> u),
                currentK + 1)
            }
            search(newQueue, newDistances, newPredecessors, newVisited, newK)
          else
            search(queue.tail, distances, predecessors, visited, k)
        case None => (distances, predecessors)
    search(TreeSet.from(sources.map(_ -> (zero, 0)))(Ordering.by(_._2)), sources.map(_ -> zero).toMap, Map.empty, Set.empty, 1)

  val map = input.toLines.map(_.map(_.asDigit))

  val (start, end) = (Vec(0, 0), Vec(map.size - 1, map.head.size - 1))

  def at(vec: Vec): Int = map(vec.i)(vec.j)

  def adjacency(ultra: Boolean)(node: State): Set[(State, Int)] =
    val forward = Set(node)
    val turned = Set(node.copy(direction = node.direction.cw), node.copy(direction = node.direction.ccw))
      .filter(node => !ultra || node.history >= 4).map(_.copy(history = 0))
    (forward ++ turned)
      .map(node => node.copy(position = node.position + node.direction, history = node.history + 1))
      .filter(node => map.indices.contains(node.position.i) && map(node.position.i).indices.contains(node.position.j))
      .filter(node => node.history <= (if ultra then 10 else 3))
      .map(node => node -> at(node.position))

  def isEnd(ultra: Boolean)(node: State): Boolean = node.position == end && (!ultra || node.history >= 4)

  def find(ultra: Boolean) =
    val (result, _) = shortestPathDijkstra(adjacency(ultra), Set(Vec(0, 1), Vec(1, 0)).map(State(start, _, 0)), isEnd(ultra))
    result.filter((k, _) => isEnd(ultra)(k)).values.min

  part(1) = find(false)

  part(2) = find(true)

}
