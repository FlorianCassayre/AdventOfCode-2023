package adventofcode.solutions

import adventofcode.Definitions.*

import scala.collection.immutable.{AbstractSeq, LinearSeq}

@main def Day19 = Day(19) { (input, part) =>

  enum Consequence:
    case Accepted
    case Rejected
    case Forward(name: String)
  import Consequence.*
  type Part = Map[String, Int]
  case class Rule(category: String, gt: Boolean, n: Int, consequence: Consequence):
    def apply(part: Part): Option[Consequence] =
      val value = part(category)
      val operator: (Int, Int) => Boolean = if gt then _ > _ else _ < _
      if operator(value, n) then Some(consequence) else None
  case class Workflow(rules: Seq[Rule], otherwise: Consequence):
    def apply(part: Part): Consequence =
      rules.view.flatMap(_(part)).headOption.getOrElse(otherwise)

  val (workflows, parts) = input.split(lineSeparator * 2).toSeq match
    case Seq(first, second) =>
      def parseConsequence(consequence: String): Consequence = consequence match
        case "A" => Accepted
        case "R" => Rejected
        case _ => Forward(consequence)
      val workflows = first.split(lineSeparator).map {
        case s"$name{$raw}" =>
          val rawSplit = raw.split(",")
          val rules = rawSplit.init.map {
            case s"$s>$n:$c" => Rule(s, true, n.toInt, parseConsequence(c))
            case s"$s<$n:$c" => Rule(s, false, n.toInt, parseConsequence(c))
          }.toSeq
          name -> Workflow(rules, parseConsequence(rawSplit.last))
      }.toMap
      val parts = second.split(lineSeparator).map {
        case s"{$vs}" => vs.split(",").map {
          case s"$c=$n" => c -> n.toInt
        }.toMap
      }.toSeq
      (workflows, parts)

  val initialWorkflow = workflows("in")

  def verdict(part: Part, workflow: Workflow): Boolean = workflow(part) match
    case Accepted => true
    case Rejected => false
    case Forward(to) => verdict(part, workflows(to))

  part(1) = parts.filter(verdict(_, initialWorkflow)).flatMap(_.values).sum

  val properties = parts.flatMap(_.keySet)

  val initialPossibilities = properties.map(_ -> (1 to 4000)).toMap

  def countAccepted(possibilities: Map[String, IndexedSeq[Int]], rules: Seq[Rule], otherwise: Consequence): Long =
    def count(consequence: Consequence, possibilities: Map[String, IndexedSeq[Int]]): Long = consequence match
      case Accepted => possibilities.values.map(_.size.toLong).product
      case Rejected => 0
      case Forward(to) =>
        val workflow = workflows(to)
        countAccepted(possibilities, workflow.rules, workflow.otherwise)
    rules match
      case head +: tail =>
        val groups = possibilities(head.category).map(v => head(Map(head.category -> v)) -> v).groupBy(_._1).view.mapValues(_.map(_._2)).toSeq
        groups.map { (consequenceOpt, values) =>
          val newPossibilities = possibilities + (head.category -> values)
          consequenceOpt match
            case Some(consequence) => count(consequence, newPossibilities)
            case None => countAccepted(newPossibilities, tail, otherwise)
        }.sum
      case _ => count(otherwise, possibilities)

  part(2) = countAccepted(initialPossibilities, initialWorkflow.rules, initialWorkflow.otherwise)

}
