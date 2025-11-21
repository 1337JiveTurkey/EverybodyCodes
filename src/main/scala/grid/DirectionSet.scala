package grid

import scala.language.implicitConversions

/**
 * Direction set containing more than one direction in a compact representation.
 * Should be a macro but we'll get there eventually.
 *
 * @param directions All the direction bit values or'd together
 */
class DirectionSet(val directions: Int) extends AnyVal with IterableOnce[Direction] {
	def contains(d: Direction): Boolean = (d != null) && (d.bitMask & directions) != 0
	def apply(d: Direction): Boolean = contains(d)
	def isEmpty: Boolean = directions == 0
	def size: Int = Integer.bitCount(directions)

	def head: Direction = Direction.fromMask(Integer.lowestOneBit(directions))

	def tail: DirectionSet = {
		if (isEmpty) {
			this
		} else {
			new DirectionSet(directions & ~Integer.lowestOneBit(directions))
		}
	}

	def foreach[U](f: Direction => U): Unit = {
		if (contains(North))      f(North)
		if (contains(Northeast))  f(Northeast)
		if (contains(East))       f(East)
		if (contains(Southeast))  f(Southeast)
		if (contains(South))      f(South)
		if (contains(Southwest))  f(Southwest)
		if (contains(West))       f(West)
		if (contains(Northwest))  f(Northwest)
	}

	def filter(f: Direction => Boolean): DirectionSet = {
		var ds = 0
		foreach(d => if(f(d)) ds |= d.bitMask)
		new DirectionSet(ds)
	}

	def &(d: DirectionSet): DirectionSet = new DirectionSet(directions & d.directions)
	def |(d: DirectionSet): DirectionSet = new DirectionSet(directions | d.directions)
	def unary_~(): DirectionSet = new DirectionSet(~directions & DirectionSet.All.directions)
	def -(d: DirectionSet): DirectionSet = new DirectionSet(directions & ~d.directions)

	override def iterator: Iterator[Direction] = new Iterator[Direction] {
		var set: DirectionSet = new DirectionSet(directions)
		override def hasNext: Boolean = !set.isEmpty

		override def next(): Direction = {
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
		val sb = new StringBuilder("DirectionSet(")
		for (direction <- iterator) {
			if (needComma) {
				sb.append(",")
			}
			sb.append(direction)
			needComma = true
		}
		sb.append(")").result()
	}
}

object DirectionSet {
	def apply(directions: Direction*): DirectionSet = {
		var dirSet = 0
		for (d <- directions) {
			dirSet |= d.bitMask
		}
		new DirectionSet(dirSet)
	}
	implicit def apply(d1: Direction): DirectionSet = {
		new DirectionSet(d1.bitMask)
	}
	def apply(d1: Direction, d2: Direction): DirectionSet = {
		new DirectionSet(d1.bitMask | d2.bitMask)
	}
	def apply(d1: Direction, d2: Direction, d3: Direction): DirectionSet = {
		new DirectionSet(d1.bitMask | d2.bitMask | d3.bitMask)
	}
	def apply(d1: Direction, d2: Direction, d3: Direction, d4: Direction): DirectionSet = {
		new DirectionSet(d1.bitMask | d2.bitMask | d3.bitMask | d4.bitMask)
	}

	val All: DirectionSet = DirectionSet(North, Northeast, East, Southeast, South, Southwest, West, Northwest)
	val Empty: DirectionSet = DirectionSet()
	val Cardinals: DirectionSet = DirectionSet(North, South, East, West)
	val Diagonals: DirectionSet = DirectionSet(Northeast, Northwest, Southeast, Southwest)
}
