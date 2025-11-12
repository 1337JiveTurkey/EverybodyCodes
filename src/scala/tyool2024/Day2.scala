package tyool2024

import grid.{Cell, DirectionSet, Grid, WrapX}

import scala.collection.mutable
import scala.util.matching.Regex

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day2Star1.txt")
		val words = lines(0).substring(6).split(",")
		val inscription = lines(2)
		var matches = 0
		for (word <- words) {
			for (fragment <- inscription.sliding(word.length)) {
				if (word == fragment) {
					matches += 1
				}
			}
		}
		println(matches)
	}

	def star2(): Unit = {
		val stanzas = splitOnBlanks(fileLines("Day2Star2.txt"))

		val patterns = createPatterns(stanzas(0)(0))
		val lines = stanzas(1)

		var total = 0
		for (line <- lines) {
			val touched = findTouchedLetters(patterns, line)
			total += touched
		}
		println(total)
	}

	private def createWords(wordsLine: String): Iterable[String] = {
		val wordSubstring = wordsLine.substring(6)
		wordSubstring.split(',')
	}

	private def createPatterns(wordsLine: String): Iterable[Regex] = {
		val wordSubstring = wordsLine.substring(6)
		val plusReversed = wordSubstring + ',' + wordSubstring.reverse
		val words = plusReversed.split(',').toSet
		words.map(_.r)
	}

	private def findTouchedLetters(patterns: Iterable[Regex], line: String): Int = {
		val touched = mutable.BitSet.empty
		touched.sizeHint(line.length)
		for (pattern <- patterns) {
			for (i <- line.indices) {
				val subLine = line.substring(i)
				for (m <- pattern.findAllMatchIn(subLine)) {
					val realStart = m.start + i
					val realEnd = m.end + i
					touched.addAll(realStart until realEnd)
				}
			}
		}
		drawCarets(line, touched)
		touched.size
	}

	private def drawCarets(line:String, bits: mutable.BitSet): Unit = {
		println(line)
		val sb = new StringBuilder()
		for (i <- line.indices) {
			if (bits(i)) {
				sb.append('^')
			} else {
				sb.append(' ')
			}
		}
		println(sb.toString())
	}

	def star3(): Unit = {
		val stanzas = splitOnBlanks(fileLines("Day2Star3.txt"))

		val words = createWords(stanzas(0)(0))
		val lines = stanzas(1)
		val grid = Grid(lines)(new Rune(_)).withEdges(WrapX)
		for (word <- words) {
			for (cell <- grid.cells) {
				matchCellToWord(word, cell)
			}
		}
		println(grid.render(_.toChar))
		println(grid.count(_.touched))
	}

	private def matchCellToWord(word: String, cell: Cell[Rune]): Unit = {
		if (cell.value.c == word.head) {
			if (word.length == 1) {
				cell.value.touched = true
			} else {
				val tail = word.tail
				val validDirections = cell.validDirections & DirectionSet.Cardinals
				for (direction <- validDirections) {
					// Send a ray in whatever direction that's as long as the rest of the word
					val ray = cell.ray(direction).take(tail.length)
					val rayChars = ray.map(_.value.c).mkString
					if (tail == rayChars) {
						cell.value.touched = true
						for (rayCell <- ray) {
							rayCell.value.touched = true
						}
					}
				}
			}
		}
	}

	private class Rune(val c: Char) {
		var touched: Boolean = false

		implicit def toChar: Char = if (touched) c else ' '
	}
}