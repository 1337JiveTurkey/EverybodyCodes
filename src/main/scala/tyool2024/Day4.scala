package tyool2024

object Day4 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day4Star1.txt")
		val numbers = lines.map(Integer.parseInt)
		val min = numbers.min
		val offsetNumbers = numbers.map(_ - min)
		println(offsetNumbers.sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day4Star2.txt")
		val numbers = lines.map(Integer.parseInt)
		val min = numbers.min
		val offsetNumbers = numbers.map(_ - min)
		println(offsetNumbers.sum)
	}

	def star3(): Unit = {
		val lines = fileLines("Day4Star3.txt")
		val numbers = lines.map(Integer.parseInt)
		var minTotal = Integer.MAX_VALUE
		for (i <- numbers.min to numbers.max) {
			var deltaSum = 0
			for (number <- numbers) {
				val delta = Math.abs(number - i)
				deltaSum += delta
			}
			minTotal = Math.min(deltaSum, minTotal)
		}
		println(minTotal)
	}

}
