package tyool2024

import grid.{DirectionSet, Grid}

object Day3 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day3Star1.txt")
		val grid = Grid(lines) {
			case '#' => 1
			case '.' => 0
		}
		for (i <- 1 to 9) {
			for (cell <- grid.cells) {
				val matches = cell.neighbors(DirectionSet.Cardinals).forall(_.value >= i)
				if (matches) {
					cell.value = i + 1
				}
			}
		}
		println(grid.sum)
	}

	def star2(): Unit = {
		val lines = fileLines("Day3Star2.txt")
		val grid = Grid(lines) {
			case '#' => 1
			case '.' => 0
		}
		for (i <- 1 to 20) {
			for (cell <- grid.cells) {
				val matches = cell.neighbors(DirectionSet.Cardinals).forall(_.value >= i)
				if (matches) {
					cell.value = i + 1
				}
			}
		}
		println(grid.sum)
	}

	def star3(): Unit = {
		val lines = fileLines("Day3Star3.txt")
		val grid = Grid(lines) {
			case '#' => 1
			case '.' => 0
		}
		for (i <- 1 to 100) {
			for (cell <- grid.cells) {
				if (!cell.onEdge) {
					val matches = cell.neighbors.forall(_.value >= i)
					if (matches) {
						cell.value = i + 1
					}
				}
			}
		}
		println(grid.render(_.toHexString.charAt(0)))
		println(grid.sum)
	}
}
