package tyool2024

import grid._

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day7Star1.txt")
		val labeledPairs = lines.map(lineToLabeledPair)
		val totals = for ((key, line) <- labeledPairs) yield {
			val powerAdjustments = repeatForever(line.split(',').map(toPowerAdjustment))
			var power = 10
			var total = 0
			for (adjustment <- powerAdjustments.take(10)) {
				power = adjustment(power)
				total += power
			}
			(key, total)
		}
		println(totals.sortBy(_._2).map(_._1).reverse.mkString)
	}

	def star2(): Unit = {
		val lines = fileLines("Day7Star2.txt")
		val racetrack = parseRacetrack(fileLines("Day7Star2Racetrack.txt"))
		val labeledPairs = lines.map(lineToLabeledPair)
		val length = racetrack.length * 10
		val fullCourse = repeatForever(racetrack).take(length)
		val totals = for ((key, line) <- labeledPairs) yield {
			val plan = repeatForever(line.split(',').map(toPowerAdjustment))
			val total = evaluatePlan(plan, fullCourse)
			println(s"$key: $total")
			(key, total)
		}
		println(totals.sortBy(_._2).map(_._1).reverse.mkString)
	}

	def star3(): Unit = {
		val (_, line) = lineToLabeledPair(fileLine("Day7Star3.txt"))
		val rivalPlan = line.split(',').map(toPowerAdjustment)

		val racetrack = parseRacetrack(fileLines("Day7Star3Racetrack.txt"))
		val length = racetrack.length * 2024
		val fullCourse = repeatForever(racetrack).take(length)

		val rivalTotal = evaluatePlan(rivalPlan, fullCourse)

		var winningPlans = 0
		val plans = validPlans()
		for (plan <- plans) {
			val total = evaluatePlan(plan, fullCourse)
			if (total > rivalTotal) {
				winningPlans += 1
			}
		}
		println(winningPlans)
	}

	def evaluatePlan(racerPlan: Seq[Adjustment], fullCourse: Iterable[Adjustment]): Long = {
		val foreverPlan = repeatForever(racerPlan).take(fullCourse.size).toArray
		val resultAdjustments = fullCourse.zip(foreverPlan).map {
			case (track, plan) => combineAdjustments(track, plan)
		}
		var power = 10
		var total: Long = 0
		for (adjustment <- resultAdjustments) {
			power = adjustment(power)
			total += power
		}
		total
	}

	def parseRacetrack(lines: IndexedSeq[String]): Seq[Adjustment] = {
		val grid = Grid(lines)(identity)
		val sb = IndexedSeq.newBuilder[Adjustment]
		val start = grid.cell(0,0)
		// Start in a direction that's not looking to the south
		var direction: Direction = North
		var cell = start
		// When we move forward these are the directions we can turn
		val possibleForwards = BearingSet(Fore, Left, Right)
		do {
			var nextCell: Cell[Char] = null
			var nextDirection: Direction = null
			// Look at directions that are valid and could lead forward on the path
			for (lookAt <- direction.relative(possibleForwards)) {
				val lookedCell = cell.get(lookAt)
				if (lookedCell.isDefined && lookedCell.get.value != ' ') {
					nextCell = lookedCell.get
					nextDirection = lookAt
				}
			}
			cell = nextCell
			direction = nextDirection
			sb.addOne(toPowerAdjustment(String.valueOf(cell.value)))
		} while (cell.value != 'S')
		val retVal = sb.result()
		println(retVal)
		retVal
	}

	def validPlans(): IndexedSeq[IndexedSeq[Adjustment]] = {
		val permutations = "+++++===---".permutations.toSet.toIndexedSeq
		for (permutation <- permutations) yield {
			permutation.map(ch => toPowerAdjustment(ch.toString))
		}
	}

	def toPowerAdjustment(ch: String): Adjustment = {
		ch match {
			case "+" => Increase
			case "=" => Maintain
			case "S" => Maintain
			case "-" => Decrease
		}
	}

	def combineAdjustments(track: Adjustment, plan: Adjustment): Adjustment = {
		if (track == Maintain)
			plan
		else
			track
	}

	trait Adjustment extends (Int => Int)
	case object Increase extends Adjustment {
		def apply(v1: Int): Int = v1 + 1

		override def toString(): String = "+"
	}
	case object Decrease extends Adjustment {
		def apply(v1: Int): Int = Math.max(v1 - 1, 0)

		override def toString(): String = "-"
	}
	case object Maintain extends Adjustment {
		def apply(v1: Int): Int = v1

		override def toString(): String = "="
	}

}
