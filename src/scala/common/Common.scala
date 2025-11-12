package common

import java.security.MessageDigest
import scala.collection.mutable
import scala.io.Source

class Common(dirPrefix: String) {
	def fileLines(filename: String): IndexedSeq[String] = {
		val file = Source.fromFile(dirPrefix + filename)
		try {
			// Fully parses the file into memory with this step so we can safely close
			file.getLines().toIndexedSeq
		} finally {
			file.close()
		}
	}

	def fileText(filename: String): String = {
		val lines = fileLines(filename)
		lines.reduce(_ + _)
	}

	def fileLine(filename: String): String = {
		val lines = fileLines(filename)
		assert(lines.length == 1)
		lines.head
	}

	def fileNumbers(filename: String): IndexedSeq[Int] = {
		val line = fileLine(filename)
		line.split(",").flatMap(_.toIntOption)
	}

	/**
	 * Reads a line with numbers in it separated by something that's not a number
	 * and splits it into an IndexedSeq containing all the numbers on the line.
	 * This handles negative numbers correctly.
	 *
	 * @param line The line of text to extract the numbers from
	 * @return The numbers on the line.
	 */
	def dividedNumbers(line: String): IndexedSeq[Int] = {
		line.split("""[^\d\-]+""").flatMap(_.toIntOption)
	}

	// Splits a string into a list of 2 character substrings
	def pairwiseStrings(string: String): Iterator[String] = {
		string.sliding(2)
	}

	def pairwise[T](list: Iterator[T]): Iterator[(T, T)] = {
		list.sliding(2).map(pair => (pair.head, pair.tail.head))
	}

	def groupedPairwise[T](list: Iterator[T]): Iterator[(T, T)] = {
		list.grouped(2).map(pair => (pair.head, pair.tail.head))
	}

	// Like pairwise but considering three adjacent elements at a time
	def tripwise[T](list: Iterator[T]): Iterator[(T, T, T)] = {
		list.sliding(3).map(pair => (pair.head, pair(1), pair(2)))
	}

	// Same as the above two but four adjacent elements
	def quadwise[T](list: Iterator[T]): Iterator[(T, T, T, T)] = {
		list.sliding(4).map(pair => (pair.head, pair(1), pair(2), pair(3)))
	}

	// Pairs split into separate lists, even-odd
	def halves[T](list: Seq[T]): (Seq[T], Seq[T]) = {
		list.grouped(2).map(pair => (pair.head, pair.tail.head)).toList.unzip
	}

	def cartesianProduct[T](list: IndexedSeq[T]): IndexedSeq[(T, T)] = {
		val retVal = IndexedSeq.newBuilder[(T, T)]
		for (x <- list.indices) {
			for (y <- list.indices) {
				retVal.addOne((list(x), list(y)))
			}
		}
		retVal.result()
	}

	// Cartesian product minus the elements (x,x)
	def allPairs[T](list: IndexedSeq[T]): IndexedSeq[(T, T)] = {
		val retVal = IndexedSeq.newBuilder[(T, T)]
		for (x <- list.indices) {
			for (y <- list.indices) {
				if (x != y) {
					retVal.addOne((list(x), list(y)))
				}
			}
		}
		retVal.result()
	}

	def allDistinctPairs[T](list: IndexedSeq[T]): IndexedSeq[(T, T)] = {
		val retVal = IndexedSeq.newBuilder[(T, T)]
		for (x <- 0 until list.length - 1) {
			for (y <- x + 1 until list.length) {
				retVal.addOne((list(x), list(y)))
			}
		}
		retVal.result()
	}

	def repeatForever[T](s: Seq[T]): LazyList[T] = {
		LazyList.continually(s.to(LazyList)).flatten
	}

	/**
	 * Generates a sequence of sequences where the nth sequence of the result is
	 * missing the nth element of the input sequence.
	 *
	 * @param s The sequence to exclude the elements of
	 * @tparam T The type of the original sequence
	 * @return A sequence of sequences, as described above
	 */
	def seqOfSeqComplements[T](s: IndexedSeq[T]): Seq[IndexedSeq[T]] = {
		for (i <- s.indices) yield {
			s.take(i) appendedAll s.drop(i+1)
		}
	}

	private lazy val md = MessageDigest.getInstance("MD5")
	// Generate MD5 hash as a hex string
	def md5(s: String): String = {
		val a = md.digest(s.getBytes)
		val sb = new mutable.StringBuilder(a.length * 2)
		for (b <- a) {
			sb.append(String.format("%02x", b))
		}
		sb.toString
	}

	// Splits an IndexedSeq of strings into stanzas based on blank lines
	def splitOnBlanks(input: IndexedSeq[String]): IndexedSeq[IndexedSeq[String]] = {
		val retVal = IndexedSeq.newBuilder[IndexedSeq[String]]
		val stanza = IndexedSeq.newBuilder[String]
		for (line <- input) {
			if (line.isBlank) {
				retVal += stanza.result()
				stanza.clear()
			}
			else {
				stanza += line
			}
		}
		val remainder = stanza.result()
		if (remainder.nonEmpty) {
			retVal += remainder
		}
		retVal.result()
	}

	def countCharacters(input: Seq[Char]): Map[Char, Int] = {
		input.groupBy(identity).view.mapValues(_.length).toMap
	}

	/**
	 *
	 * @param char The character to turn into a digit.
	 * @return The character as a digit.
	 */
	def toDigit(char: Char): Int = {
		Integer.parseInt(String.valueOf(char))
	}

	/**
	 *
	 * @param n The number to find the digits of.
	 * @return The digits of the given number from least significant to most.
	 */
	def digits(n: Int): List[Int] = {
		if (n == 0) {
			return List(0)
		}
		val lb = List.newBuilder[Int]
		var r = n
		while (r != 0) {
			lb.addOne(r % 10)
			r = r / 10
		}
		lb.result()
	}

	/**
	 *
	 * @param n The number to get the bits of.
	 * @return The numbers of the bits turned on for the given number.
	 */
	def bits(n: Int): List[Int] = {
		var l: List[Int] = Nil
		for (i <- (0 until 32).reverse) {
			val bit: Int = 1 << i
			if ((n & bit) > 0) {
				l = i :: l
			}
		}
		l
	}

}
