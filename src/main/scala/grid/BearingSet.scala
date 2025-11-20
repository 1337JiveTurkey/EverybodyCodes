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


	def head: Bearing = Bearing.fromMask(Integer.highestOneBit(bearings))

	def tail: BearingSet = {
		if (isEmpty) {
			this
		} else {
			new BearingSet(bearings & ~Integer.highestOneBit(bearings))
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
		var ds = 0
		foreach(d => if(f(d)) ds |= d.bitMask)
		new BearingSet(ds)
	}

	def &(d: BearingSet): BearingSet = new BearingSet(bearings & d.bearings)
	def |(d: BearingSet): BearingSet = new BearingSet(bearings | d.bearings)
	def unary_~(): BearingSet = new BearingSet(~bearings & BearingSet.All.bearings)
	def -(d: BearingSet): BearingSet = new BearingSet(bearings & ~d.bearings)


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
		for (d <- Bearings) {
			bearingSet |= d.bitMask
		}
		new BearingSet(bearingSet)
	}
	implicit def apply(d1: Bearing): BearingSet = {
		new BearingSet(d1.bitMask)
	}
	def apply(d1: Bearing, d2: Bearing): BearingSet = {
		new BearingSet(d1.bitMask | d2.bitMask)
	}
	def apply(d1: Bearing, d2: Bearing, d3: Bearing): BearingSet = {
		new BearingSet(d1.bitMask | d2.bitMask | d3.bitMask)
	}
	def apply(d1: Bearing, d2: Bearing, d3: Bearing, d4: Bearing): BearingSet = {
		new BearingSet(d1.bitMask | d2.bitMask | d3.bitMask | d4.bitMask)
	}

	val All: BearingSet = BearingSet(Fore, ForeRight, Right, BackRight, Back, BackLeft, Left, ForeLeft)
	val Empty: BearingSet = BearingSet()
	val Cardinals: BearingSet = BearingSet(Fore, Right, Back, Left)
	val Diagonals: BearingSet = BearingSet(ForeRight, BackRight, BackLeft, ForeLeft)
}
