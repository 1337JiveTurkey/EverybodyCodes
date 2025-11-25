package tyool2024

object Day11 extends Main {
	def main (args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val lines = fileLines("Day11Star1.txt").map(lineToLabeledPair)
		val births = Map.from(for ((label, sequenceText) <- lines) yield {
			(label.charAt(0), sequenceText.split(',').mkString)
		})
		var generation = "A"
		for (i <- 1 to 4) {
			generation = calculateDay(generation, births)
		}
		println(generation)
		println(generation.length)
	}

	def star2(): Unit = {
		val lines = fileLines("Day11Star2.txt").map(lineToLabeledPair)
		val births = Map.from(for ((label, sequenceText) <- lines) yield {
			(label.charAt(0), sequenceText.split(',').mkString)
		})
		var generation = "Z"
		for (i <- 1 to 10) {
			generation = calculateDay(generation, births)
		}
		println(generation)
		println(generation.length)
	}


	def calculateDay(generation: String, births: Map[Char, String]): String = {
		val sb = new StringBuilder()
		for (termite <- generation) {
			sb.append(births(termite))
		}
		sb.result()
	}
}
