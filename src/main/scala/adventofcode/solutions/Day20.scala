package adventofcode.solutions

import adventofcode.Definitions.*

@main def Day20 = Day(20) { (input, part) =>

  enum Module:
    case FlipFlop(state: Boolean)
    case Conjunction(last: Map[String, Boolean])
    case Broadcast
  import Module.*

  val broadcaster = "broadcaster"

  val modulesUninitialized: Map[String, (Module, Seq[String])] = input.toLines.map {
    case s"$source -> $destinations" =>
      val (name, module) = source match
        case `broadcaster` => broadcaster -> Broadcast
        case s"%$name" => name -> FlipFlop(false)
        case s"&$name" => name -> Conjunction(Map.empty)
      name -> (module, destinations.split(", ").toSeq)
  }.toMap
  val inputs = modulesUninitialized.toSeq.flatMap { case (input, (_, outputs)) => outputs.map(_ -> input) }.groupBy(_._1)
    .view.mapValues(_.map(_._2)).toMap.withDefaultValue(Seq.empty)
  val outputs = modulesUninitialized.view.map { case (input, (_, outputs)) => input -> outputs }.toMap
  val initialState: Map[String, Module] = modulesUninitialized.map { case (name, (module, _)) =>
    val newModule = module match
      case Conjunction(_) => Conjunction(inputs(name).map(_ -> false).toMap)
      case _ => module
    name -> newModule
  }

  val rx = "rx"

  def simulate(queue: Vector[(String, Boolean, String)], state: Map[String, Module], pulses: Map[Boolean, Int], highs: Set[String]): (Map[String, Module], Map[Boolean, Int], Set[String]) = queue match
    case (from, signal, to) +: tail =>
      val newPulses = pulses + (signal -> (pulses(signal) + 1))
      val newHighs = highs ++ (if !signal then Set(to) else Set.empty)
      state.get(to) match
        case Some(module) =>
          val output = outputs(to)
          val (newModule, newSignal) = module match
            case FlipFlop(state) => signal match
              case false =>
                val newState = !state
                (FlipFlop(newState), Some(newState))
              case true => (module, None)
            case Conjunction(last) =>
              val newLast = last + (from -> signal)
              val newPulse = !newLast.values.forall(identity)
              (Conjunction(newLast), Some(newPulse))
            case Broadcast => (module, Some(signal))
            val newSignals = newSignal.map(s => outputs(to).map(o => (to, s, o)).toVector).getOrElse(Vector.empty)
          simulate(tail ++ newSignals, state + (to -> newModule), newPulses, newHighs)
        case None => simulate(tail, state, newPulses, newHighs)
    case _ => (state, pulses, highs)

  def simulateButton(state: Map[String, Module], pulses: Map[Boolean, Int]): (Map[String, Module], Map[Boolean, Int], Set[String]) =
    simulate(Vector(("button", false, broadcaster)), state, pulses, Set.empty)

  def simulateMany(state: Map[String, Module], pulses: Map[Boolean, Int], i: Int): Long =
    if i > 0 then
      val (newState, newPulses, _) = simulateButton(state, pulses)
      simulateMany(newState, newPulses, i - 1)
    else
      pulses.values.map(_.toLong).product

  val initialPulses = Map.empty[Boolean, Int].withDefaultValue(0)

  part(1) = simulateMany(initialState, initialPulses, 1000)

  val interest = inputs(rx).flatMap(inputs).toSet

  def findFrequencies(state: Map[String, Module], i: Int, all: Map[String, Int]): Long =
    val (newState, _, highs) = simulateButton(state, initialPulses)
    val lasts = interest.flatMap(inputs).map(newState).collect { case Conjunction(last) => last }
    val newI = i + 1
    val newAll = all ++ highs.intersect(interest).diff(all.keySet).map(_ -> newI)
    if newAll.keySet == interest then newAll.values.map(_.toLong).product else findFrequencies(newState, i + 1, newAll)

  part(2) = findFrequencies(initialState, 0, Map.empty)

}
