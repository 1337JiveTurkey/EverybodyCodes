package tyool2024

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val line = fileLine("Day1Star1.txt")
		val potions = line.map({
			case 'A' => 0
			case 'B' => 1
			case 'C' => 3
		})
		println(potions.sum)
	}

	def star2(): Unit = {
		val line = fileLine("Day1Star2.txt")
		val pairs = line.grouped(2)
		var total = 0
		for (pair <- pairs) {
			val potions = pair.map({
				case 'x' => -1
				case 'A' => 1
				case 'B' => 2
				case 'C' => 4
				case 'D' => 6
			}).sum
			total += Math.max(potions, 0)
		}
		println(total)
	}

	def star3(): Unit = {
		val line = fileLine("Day1Star3.txt")
		val triples = line.grouped(3)
		var total = 0
		for (triple <- triples) {
			val monsters = triple.map(potionCost)
			var count = 0
			for (monster <- monsters) {
				if (monster.isDefined) {
					count += 1
					total += monster.get
				}
			}
			if (count == 2) {
				total += 2
			}
			if (count == 3) {
				total += 6
			}
		}
		println(total)
	}

	def potionCost(ch: Char): Option[Int] = ch match {
		case 'x' => None
		case 'A' => Some(0)
		case 'B' => Some(1)
		case 'C' => Some(3)
		case 'D' => Some(5)
	}
}
