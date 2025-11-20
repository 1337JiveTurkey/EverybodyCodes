package tyool2025

import grid.Grid

object Day2 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val line = fileLine("Day2Star1.txt")
		val a = CxNumber(line)
		val t = CxNumber(10, 10)
		var r = CxNumber(0, 0)
		for (i <- 1 to 3) {
			r *= r
			r /= t
			r += a
		}

		println(r)
	}

	def star2(): Unit = {
		val line = fileLine("Day2Star2.txt")
		val upperLeftA = CxNumber(line)
		val grid = new Grid[Boolean](101, 101)
		val n = CxNumber(100000,100000)
		for (cell <- grid.cells) {
			cell.value = true
			val a = upperLeftA + CxNumber(cell.x * 10, cell.y * 10)
			var r = CxNumber(0, 0)
			for (i <- 1 to 100) {
				r *= r
				r /= n
				r += a
				if (!r.inBounds) {
					cell.value = false
				}
			}
		}
		println(grid.render(Grid.renderBoolean))
		println(grid.count(identity))
	}

	def star3(): Unit = {
		val line = fileLine("Day2Star3.txt")
		val upperLeftA = CxNumber(line)
		val grid = new Grid[Boolean](1001, 1001)
		val n = CxNumber(100000,100000)
		for (cell <- grid.cells) {
			cell.value = true
			val a = upperLeftA + CxNumber(cell.x, cell.y)
			var r = CxNumber(0, 0)
			for (i <- 1 to 100) {
				r *= r
				r /= n
				r += a
				if (!r.inBounds) {
					cell.value = false
				}
			}
		}
//		println(grid.render(Grid.renderBoolean))
		println(grid.count(identity))
	}

	case class CxNumber(x: Long, y: Long) {

		def sum(other: CxNumber): CxNumber = CxNumber(x + other.x, y + other.y)
		def +(other: CxNumber): CxNumber = sum(other)
		def difference(other: CxNumber): CxNumber = CxNumber(x - other.x, y - other.y)
		def -(other: CxNumber): CxNumber = difference(other)
		def product(other: CxNumber): CxNumber = CxNumber(x * other.x - y * other.y, x * other.y + y * other.x)
		def *(other: CxNumber): CxNumber = product(other)
		def quotient(other: CxNumber): CxNumber = CxNumber(x / other.x, y / other.y)
		def /(other: CxNumber): CxNumber = quotient(other)

		override def toString: String = s"[$x,$y]"

		def inBounds: Boolean = x >= -1000000L && x <= 1000000L && y>= -1000000L && y <= 1000000L
	}

	object CxNumber {
		def apply(fromString: String): CxNumber = {
			val numbers = dividedNumbers(fromString)
			CxNumber(numbers(0), numbers(1))
		}
	}
}
