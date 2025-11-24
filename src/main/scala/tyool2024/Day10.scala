package tyool2024

import grid.Grid

object Day10 extends Main {
	def main(args: Array[String]): Unit = {
		star2()
	}

	def star1(): Unit = {
		val grid = Grid(fileLines("Day10Star1.txt"))(identity)
		println(processBlock(grid))
	}

	def star2(): Unit = {
		val overallGrid = Grid(fileLines("Day10Star2.txt"))(identity)
		var powerSum = 0
		for (y <- overallGrid.yIndices by 9) {
			for (x <- overallGrid.xIndices by 9) {
				val grid = overallGrid.subGrid(x until x + 8, y until y + 8)
				val runes = processBlock(grid)
				powerSum += runesToPower(runes)
			}
		}
		println(powerSum)
	}

	def processBlock(grid: Grid[Char]): String = {
		val outside = Array(0, 1, 6, 7)
		val inside  = Array(2, 3, 4, 5)

		val sb = new StringBuilder()
		// Inside is the inside four, outside is the outside four
		for (y <- inside) {
			val rowValues = outside.map(x => grid(x, y)).toSet
			for (x <- inside) {
				val colValues = outside.map(y => grid(x, y)).toSet
				val char = rowValues.intersect(colValues).head
				grid(x, y) = char
				sb.append(char)
			}
		}
		sb.result()
	}

	def runesToPower(runes: String): Int = {
		val powers = for ((rune, position) <- runes.zipWithIndex) yield {
			(rune - 'A' + 1) * (position + 1)
		}
		powers.sum
	}
}
