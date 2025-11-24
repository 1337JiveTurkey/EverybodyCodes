package tyool2024

import grid.Grid

object Day10 extends Main {
	def main(args: Array[String]): Unit = {
		star1()
	}

	def star1(): Unit = {
		val grid = Grid(fileLines("Day10Star1.txt"))(identity)
		// Inside is the inside four, outside is the outside four
		val outside = Array(0, 1, 6, 7)
		val inside  = Array(2, 3, 4, 5)

		val sb = new StringBuilder()
		for (y <- inside) {
			val rowValues = outside.map(x => grid(x, y)).toSet
			for (x <- inside) {
				val colValues = outside.map(y => grid(x, y)).toSet
				val char = rowValues.intersect(colValues).head
				grid(x, y) = char
				sb.append(char)
			}
		}
		println(sb.result())
	}
}
