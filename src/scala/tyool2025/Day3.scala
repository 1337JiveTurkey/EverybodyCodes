package tyool2025

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val numbers = fileNumbers("Day3Star1.txt")
		val distinctNumbers = numbers.toSet.toIndexedSeq.sorted
		println(distinctNumbers.sum)
	}

	def star2(): Unit = {
		val numbers = fileNumbers("Day3Star2.txt")
		val sortedNumbers = numbers.toSet.toIndexedSeq.sorted
		println(sortedNumbers.take(20).sum)
	}

	def star3(): Unit = {
		val numbers = fileNumbers("Day3Star3.txt")
		println(countValues(numbers).maxBy(_._2))
	}
}
