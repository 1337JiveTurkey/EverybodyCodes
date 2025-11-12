package grid

import scala.language.implicitConversions

/**
 * Directions as a case class
 *
 * @param dx Change in the x value to move in this direction
 * @param dy Change in the y value to move in this direction
 */
sealed abstract case class Direction(ord: Int, dx: Int, dy: Int) {
	val cardinal: Boolean = (dx & dy) == 0
	val diagonal: Boolean = !cardinal
	val horizontal: Boolean = dx != 0 && dy == 0
	val vertical: Boolean = dx == 0 && dy != 0
	val bitMask: Int = 1 << ord

	def relative(b: Bearing): Direction = Direction((ord + b.dOrd) % 8)

	def &(d: Direction): DirectionSet = DirectionSet(this) & d
	def |(d: Direction): DirectionSet = DirectionSet(this) | d
	def unary_~(): DirectionSet = ~DirectionSet(this)
}

object North      extends Direction(0,  0, -1)
object Northeast  extends Direction(1,  1, -1)
object East       extends Direction(2,  1,  0)
object Southeast  extends Direction(3,  1,  1)
object South      extends Direction(4,  0,  1)
object Southwest  extends Direction(5, -1,  1)
object West       extends Direction(6, -1,  0)
object Northwest  extends Direction(7, -1, -1)

object Direction {
	def apply(ord: Int): Direction = ord match {
		case 0 => North
		case 1 => Northeast
		case 2 => East
		case 3 => Southeast
		case 4 => South
		case 5 => Southwest
		case 6 => West
		case 7 => Northwest
		case other => throw new IndexOutOfBoundsException("Value = " + other)
	}

	def fromMask(mask: Int): Direction = mask match {
		case 1   => North
		case 2   => Northeast
		case 4   => East
		case 8   => Southeast
		case 16  => South
		case 32  => Southwest
		case 64  => West
		case 128 => Northwest
		case other => throw new IndexOutOfBoundsException("Value = " + other)
	}

	/**
	 * Turns a difference in coordinates into the relative direction.
	 *
	 * @param dx Difference in x coordinates
	 * @param dy Difference in y coordinates
	 * @return The direction to move if any
	 */
	def fromDelta(dx: Int, dy: Int): Direction = (Integer.signum(dx), Integer.signum(dy)) match {
		case ( 0, -1) => North
		case ( 1, -1) => Northeast
		case ( 1,  0) => East
		case ( 1,  1) => Southeast
		case ( 0,  1) => South
		case (-1,  1) => Southwest
		case (-1,  0) => West
		case (-1, -1) => Northwest
		case _ => null
	}
}

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

	def head: Direction = Direction.fromMask(Integer.highestOneBit(directions))

	def tail: DirectionSet = {
		if (isEmpty) {
			this
		} else {
			new DirectionSet(directions & ~Integer.highestOneBit(directions))
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

	/**
	 * Turns the set into a list ordered by which directions are closest to the
	 * priority direction in descending order.

	 * @param d The direction to prioritize
	 * @return
	 */
	def prioritized(d: Direction): Seq[Direction] = {
		val s = Seq.newBuilder[Direction]
		if (this contains d) {
			s += d
		}
		if (this contains d.relative(ForeLeft)) {
			s += d.relative(ForeLeft)
		}
		if (this contains d.relative(ForeRight)) {
			s += d.relative(ForeRight)
		}
		if (this contains d.relative(Left)) {
			s += d.relative(Left)
		}
		if (this contains d.relative(Right)) {
			s += d.relative(Right)
		}
		if (this contains d.relative(BackLeft)) {
			s += d.relative(BackLeft)
		}
		if (this contains d.relative(BackRight)) {
			s += d.relative(BackRight)
		}
		if (this contains d.relative(Back)) {
			s += d.relative(Back)
		}
		s.result()
	}

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

/**
 * Directions relative to a given direction given as a case class.
 *
 * @param dOrd The ordinal of the object, selected for easy math.
 */
sealed abstract case class Bearing(dOrd: Int) {
	val bitMask: Int = 1 << dOrd
}

object Fore extends Bearing(0)
object ForeRight extends Bearing(1)
object Right extends Bearing(2)
object BackRight extends Bearing(3)
object Back extends Bearing(4)
object BackLeft extends Bearing(5)
object Left extends Bearing(6)
object ForeLeft extends Bearing(7)

/**
 * Bearing set containing more than one Bearing in a compact representation.
 *
 * @param Bearings All the Bearing bit values or'd together
 */
class BearingSet(val Bearings: Int) extends AnyVal {
	def contains(d: Bearing): Boolean = (d != null) && (d.bitMask & Bearings) != 0
	def apply(d: Bearing): Boolean = contains(d)
	def isEmpty: Boolean = Bearings == 0

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

	def &(d: BearingSet): BearingSet = new BearingSet(Bearings & d.Bearings)
	def |(d: BearingSet): BearingSet = new BearingSet(Bearings | d.Bearings)
	def unary_~(): BearingSet = new BearingSet(~Bearings & BearingSet.All.Bearings)
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
