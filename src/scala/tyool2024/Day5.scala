package tyool2024

import scala.collection.mutable

object Day5 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day5Star1.txt")
		val numberRows = lines.map(dividedNumbers)
		val numberColumns = numberRows.transpose.map(_.to(mutable.ListBuffer))
		for (i <- 1 to 10) {
			println(round(numberColumns, i))
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day5Star2.txt")
		val numberRows = lines.map(dividedNumbers)
		val numberColumns = numberRows.transpose.map(_.to(mutable.ListBuffer))
		var i = 0
		val counts = new mutable.HashMap[String, Int]()
		do {
			i += 1
			val number = round(numberColumns, i)
			val count = counts.getOrElse(number, 0) + 1
			counts.put(number, count)
			if (count == 2024) {
				println(number)
				println(i)
				println(count)
				return
			}
		} while (true)
	}

	def star3(): Unit = {
		val lines = fileLines("Day5Star3.txt")
		val numberRows = lines.map(dividedNumbers)
		val numberColumns = numberRows.transpose.map(_.to(mutable.ListBuffer))
		var i = 0
		var maxFound: Long = 0
		do {
			i += 1
			val number = round(numberColumns, i).toLong
			maxFound = Math.max(number, maxFound)
			// Yes this is the lazy way to do it
		} while (i < 1_000_000_000)
		println(maxFound)
	}

	def round(numberColumns: IndexedSeq[mutable.ListBuffer[Int]], i: Int): String = {
		val fromColumn = (i - 1) % numberColumns.length
		val toColumn = i % numberColumns.length
		absorbColumn(numberColumns(fromColumn), numberColumns(toColumn))
		numberColumns.map(_.head).mkString
	}

	def absorbColumn(from: mutable.ListBuffer[Int], to: mutable.ListBuffer[Int]): Unit = {
		val element = from.remove(0)
		// After however many claps, do we end up on the left hand side of the column?
		// For column of 2, 1-2 claps will end up on left hand, 3-4 claps on the right hand
		val leftSide = ((element - 1) / to.length) % 2 == 0
		// Number of claps left over
		val claps = (element - 1) % to.length
		if (leftSide) {
			to.insert(claps, element)
		} else {
			to.insert(to.length - claps, element)
		}
	}
}
