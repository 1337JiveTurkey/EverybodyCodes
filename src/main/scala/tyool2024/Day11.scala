package tyool2024

import common.Counts

import java.util.concurrent.CountDownLatch

object Day11 extends Main {
	def main (args: Array[String]): Unit = {
		star3()
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

	def star3(): Unit = {
		val lines = fileLines("Day11Star3.txt").map(lineToLabeledPair)
		val births = Map.from(for ((label, sequenceText) <- lines) yield {
			(label, sequenceText.split(','))
		})
		val progenitors = births.keys

		val countsByProgenitor = for (progenitor <- progenitors) yield {
			var currentGeneration = new Counts[String]
			currentGeneration.addTo(progenitor, 1)
			for (i <- 1 to 20) {
				val nextGeneration = new Counts[String]
				for ((breed, currentCount) <- currentGeneration) {
					for (childBreed <- births(breed)) {
						nextGeneration.addTo(childBreed, currentCount)
					}
				}
				currentGeneration = nextGeneration
			}
			(progenitor, currentGeneration.sum)
		}
		for ((progenitor, count) <- countsByProgenitor) {
			println(s"$progenitor = $count")
		}
		val (maxLabel, max) = countsByProgenitor.maxBy(_._2)
		val (minLabel, min) = countsByProgenitor.minBy(_._2)
		println(max - min)
	}

	def calculateDay(generation: String, births: Map[Char, String]): String = {
		val sb = new StringBuilder()
		for (termite <- generation) {
			sb.append(births(termite))
		}
		sb.result()
	}
}
