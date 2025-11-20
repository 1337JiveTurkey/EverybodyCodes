package tyool2025

object Day1 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day1Star1.txt")
		val names = lines(0).split(',')
		val instructions = lines(2).split(',')
		var i = 0
		for (instruction <- instructions) {
			val dir = instruction.charAt(0)
			val distance = toDigit(instruction.charAt(1))
			if (dir == 'L')
				i = Math.max(0, i - distance)
			if (dir == 'R')
				i = Math.min(names.length - 1, i + distance)
		}
		println(names(i))
	}

	def star2(): Unit = {
		val lines = fileLines("Day1Star2.txt")
		val names = lines(0).split(',')
		val instructions = lines(2).split(',')
		var i = 0
		for (instruction <- instructions) {
			val dir = instruction.charAt(0)
			val distance = instruction.tail.toInt
			if (dir == 'L')
				i -= distance
			if (dir == 'R')
				i += distance
		}
		println(names(((i % names.length) + names.length) % names.length))
	}

	def star3(): Unit = {
		val lines = fileLines("Day1Star3.txt")
		val names = lines(0).split(',')
		val instructions = lines(2).split(',')

		for (instruction <- instructions) {
			val dir = instruction.charAt(0)
			val distance = instruction.tail.toInt
			var i = 0
			if (dir == 'L')
				i -= distance
			if (dir == 'R')
				i += distance
			i = ((i % names.length) + names.length) % names.length
			val swap = names(i)
			names(i) = names(0)
			names(0) = swap
		}
		println(names(0))
	}

}
