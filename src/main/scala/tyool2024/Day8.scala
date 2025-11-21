package tyool2024

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val fileNumber = fileLine("Day8Star1.txt").toInt
		var odds = 1
		var total = odds
		// This is basically finding the next square number after fileNumber
		do {
			odds += 2
			total += odds
		} while(total < fileNumber)
		println((total - fileNumber) * odds)
	}
}
