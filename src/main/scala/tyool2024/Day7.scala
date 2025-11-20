package tyool2024

import grid._

object Day7 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
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
			val planAdjustments = repeatForever(line.split(',').map(toPowerAdjustment))
			val resultAdjustments = fullCourse.zip(planAdjustments).map {
				case (track, plan) => combineAdjustments(track, plan)
			}
			var power = 10
			var total = 0
			for (adjustment <- resultAdjustments) {
				power = adjustment(power)
				total += power
			}
			println(s"$key: $total")
			(key, total)
		}
		println(totals.sortBy(_._2).map(_._1).reverse.mkString)
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
		sb.result()
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
