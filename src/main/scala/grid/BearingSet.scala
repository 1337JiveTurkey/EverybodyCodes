package grid

import scala.language.implicitConversions

/**
 * Bearing set containing more than one Bearing in a compact representation.
 *
 * @param bearings All the Bearing bit values or'd together
 */
class BearingSet(val bearings: Int) extends AnyVal with IterableOnce[Bearing] {
	def contains(d: Bearing): Boolean = (d != null) && (d.bitMask & bearings) != 0
	def apply(d: Bearing): Boolean = contains(d)
	def isEmpty: Boolean = bearings == 0
	def size: Int = Integer.bitCount(bearings)

	def head: Bearing = Bearing.fromMask(Integer.lowestOneBit(bearings))

	def tail: BearingSet = {
		if (isEmpty) {
			this
		} else {
			new BearingSet(bearings & ~Integer.lowestOneBit(bearings))
		}
	}

	def foreach[U](f: Bearing => U): Unit = {
		if (contains(Fore))       f(Fore)
		if (contains(ForeRight))  f(ForeRight)
		if (contains(Right))      f(Right)
		if (contains(BackRight))  f(BackRight)
		if (contains(Back))       f(Back)
		if (contains(BackLeft))   f(BackLeft)
		if (contains(Left))       f(Left)
		if (contains(ForeLeft))   f(ForeLeft)
	}

	def filter(f: Bearing => Boolean): BearingSet = {
		var bs = 0
		foreach(b => if(f(b)) bs |= b.bitMask)
		new BearingSet(bs)
	}

	def &(b: BearingSet): BearingSet = new BearingSet(bearings & b.bearings)
	def |(b: BearingSet): BearingSet = new BearingSet(bearings | b.bearings)
	def unary_~(): BearingSet = new BearingSet(~bearings & BearingSet.All.bearings)
	def -(b: BearingSet): BearingSet = new BearingSet(bearings & ~b.bearings)

	override def iterator: Iterator[Bearing] = new Iterator[Bearing] {
		var set: BearingSet = new BearingSet(bearings)
		override def hasNext: Boolean = !set.isEmpty

		override def next(): Bearing = {
			val oldSet = set
			if (set.isEmpty) {
				throw new IllegalStateException("Empty iterator")
			}
			set = set.tail
			oldSet.head
		}
	}

	override def toString: String = {
		var needComma = false
		val sb = new StringBuilder("BearingSet(")
		for (bearing <- iterator) {
			if (needComma) {
				sb.append(",")
			}
			sb.append(bearing)
			needComma = true
		}
		sb.append(")").result()
	}

}

object BearingSet {
	def apply(Bearings: Bearing*): BearingSet = {
		var bearingSet = 0
		for (b <- Bearings) {
			bearingSet |= b.bitMask
		}
		new BearingSet(bearingSet)
	}
	implicit def apply(b1: Bearing): BearingSet = {
		new BearingSet(b1.bitMask)
	}
	def apply(b1: Bearing, b2: Bearing): BearingSet = {
		new BearingSet(b1.bitMask | b2.bitMask)
	}
	def apply(b1: Bearing, b2: Bearing, b3: Bearing): BearingSet = {
		new BearingSet(b1.bitMask | b2.bitMask | b3.bitMask)
	}
	def apply(b1: Bearing, b2: Bearing, b3: Bearing, b4: Bearing): BearingSet = {
		new BearingSet(b1.bitMask | b2.bitMask | b3.bitMask | b4.bitMask)
	}

	val All: BearingSet = BearingSet(Fore, ForeRight, Right, BackRight, Back, BackLeft, Left, ForeLeft)
	val Empty: BearingSet = BearingSet()
	val Cardinals: BearingSet = BearingSet(Fore, Right, Back, Left)
	val Diagonals: BearingSet = BearingSet(ForeRight, BackRight, BackLeft, ForeLeft)
}
