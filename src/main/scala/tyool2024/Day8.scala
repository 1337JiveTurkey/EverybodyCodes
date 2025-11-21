package tyool2024

object Day8 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
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

	def star2(): Unit = {
		val priests = fileLine("Day8Star2.txt").toInt
		val acolytes = 1111
		val blocks = 20240000
		var layer = 1
		var width = 1
		var total = width
		var thickness = 1
		do {
			layer += 1
			width = (2 * layer) - 1
			thickness = (thickness * priests) % acolytes
			total += width * thickness
			println(s"Layer $layer, Width $width, Thickness $thickness, Total $total")
		} while(total < blocks)
		val remaining = total - blocks
		println(s"Remaining $remaining, Width $width")
		println(remaining * width)
	}

	def star3(): Unit = {
		val highPriests = fileLine("Day8Star3.txt").toInt
		val highPriestAcolytes = 10
		val blocks = 202400000
		var layer = 1
		var width = 1
		var total = width
		var thickness = 1
		do {
			layer += 1
			width = (2 * layer) - 1
			thickness = (thickness * highPriests) % highPriestAcolytes + highPriestAcolytes
			total += width * thickness
			println(s"Layer $layer, Width $width, Thickness $thickness, Total $total")
		} while(total < blocks)
		val remaining = total - blocks
		println(s"Remaining $remaining, Width $width")
		println(remaining * width)
	}
}
