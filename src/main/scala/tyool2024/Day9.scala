package tyool2024

object Day9 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val fileNumbers = fileLines("Day9Star1.txt").map(_.toInt)
		val stamps = dividedNumbers("1, 3, 5, 10").reverse

		var beetles = 0
		for (number <- fileNumbers) {
			var remainder = number
			for (stamp <- stamps) {
				beetles += remainder / stamp
				remainder = remainder % stamp
			}
		}
		println(beetles)
	}

	def star2(): Unit = {
		val fileNumbers = fileLines("Day9Star2.txt").map(_.toInt)
		val stamps = dividedNumbers("1, 3, 5, 10, 15, 16, 20, 24, 25, 30").reverse

		val maxNumber = fileNumbers.max
		val required = precomputeStamps(stamps, maxNumber)
		var beetles = 0
		for (number <- fileNumbers) {
			beetles += required(number)
		}
		println(beetles)
	}

	def star3(): Unit = {
		val fileNumbers = fileLines("Day9Star3.txt").map(_.toInt)
		val stamps = dividedNumbers("1, 3, 5, 10, 15, 16, 20, 24, 25, 30, 37, 38, 49, 50, 74, 75, 100, 101").reverse

		val maxNumber = fileNumbers.max
		val required = precomputeStamps(stamps, maxNumber)

		var totalBeetles = 0
		for (number <- fileNumbers) {
			val half = number / 2
			var minimumBeetles = Int.MaxValue
			for (halfish <- (half - 50) to (half + 50)) {
				val otherHalfish = number - halfish
				// Something with odd numbers I think might happen
				if (Math.abs(halfish - otherHalfish) <= 100) {
					minimumBeetles = Math.min(minimumBeetles, required(halfish) + required(otherHalfish))
				}
			}
			totalBeetles += minimumBeetles
		}
		println(totalBeetles)
	}

	private def precomputeStamps(stamps: IndexedSeq[Int], maxNumber: Int): Array[Int] = {
		val required = new Array[Int](maxNumber + 1)
		for (i <- required.indices) {
			if (i == 0) {
				required(i) = 0
			}
			else {
				var minStamps = Int.MaxValue
				for (stamp <- stamps) {
					if (i - stamp >= 0) {
						minStamps = Math.min(minStamps, required(i - stamp) + 1)
					}
				}
				assert(minStamps < Int.MaxValue)
				required(i) = minStamps
			}
		}
		required
	}
}
