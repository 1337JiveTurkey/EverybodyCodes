package grid

import scala.language.implicitConversions

/**
 * Directions as a case class
 *
 * @param dx Change in the x value to move in this direction
 * @param dy Change in the y value to move in this direction
 */
sealed abstract case class Direction(ord: Int, dx: Int, dy: Int, name: String) {
	val cardinal: Boolean = (dx & dy) == 0
	val diagonal: Boolean = !cardinal
	val horizontal: Boolean = dx != 0 && dy == 0
	val vertical: Boolean = dx == 0 && dy != 0
	val bitMask: Int = 1 << ord

	override def toString(): String = name
	/**
	 * Turn a relative bearing into an absolute direction.
	 * @param b The bearing.
	 * @return The direction that the bearing points in relative to this direction.
	 */
	def relative(b: Bearing): Direction = Direction((ord + b.dOrd) % 8)

	/**
	 * Turn a set of relative bearings into a set of absolute directions.
	 * @param bs The bearings relative to the current direction.
	 * @return The absolute directions.
	 */
	def relative(bs: BearingSet): DirectionSet = {
		val bits = bs.bearings & 0xff
		new DirectionSet(bits >> ord | (bits << (8 - ord) & 0xff))
	}

	def &(d: Direction): DirectionSet = DirectionSet(this) & d
	def |(d: Direction): DirectionSet = DirectionSet(this) | d
	def unary_~(): DirectionSet = ~DirectionSet(this)
}

object North      extends Direction(0,  0, -1, "North")
object Northeast  extends Direction(1,  1, -1, "Northeast")
object East       extends Direction(2,  1,  0, "East")
object Southeast  extends Direction(3,  1,  1, "Southeast")
object South      extends Direction(4,  0,  1, "South")
object Southwest  extends Direction(5, -1,  1, "Southwest")
object West       extends Direction(6, -1,  0, "West")
object Northwest  extends Direction(7, -1, -1, "Northwest")

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

	override def toString: String = {
		var needComma = false
		val sb = new StringBuilder("DirectionSet(")
		for (direction <- DirectionSet.All) {
			if (this.contains(direction)) {
				if (needComma) {
					sb.append(",")
				}
				sb.append(direction)
				needComma = true
			}
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

/**
 * Directions relative to a given direction given as a case class.
 *
 * @param dOrd The ordinal of the object, selected for easy math.
 */
sealed abstract case class Bearing(dOrd: Int, name: String) {
	val bitMask: Int = 1 << dOrd
}

object Fore       extends Bearing(0, "Fore")
object ForeRight  extends Bearing(1, "ForeRight")
object Right      extends Bearing(2, "Right")
object BackRight  extends Bearing(3, "BackRight")
object Back       extends Bearing(4, "Back")
object BackLeft   extends Bearing(5, "BackLeft")
object Left       extends Bearing(6, "Left")
object ForeLeft   extends Bearing(7, "ForeLeft")

object Bearing {
	def apply(ord: Int): Bearing = ord match {
		case 0 => Fore
		case 1 => ForeRight
		case 2 => Right
		case 3 => BackRight
		case 4 => Back
		case 5 => BackLeft
		case 6 => Left
		case 7 => ForeLeft
		case other => throw new IndexOutOfBoundsException("Value = " + other)
	}

	def fromMask(mask: Int): Bearing = mask match {
		case 1   => Fore
		case 2   => ForeRight
		case 4   => Right
		case 8   => BackRight
		case 16  => Back
		case 32  => BackLeft
		case 64  => Left
		case 128 => ForeLeft
		case other => throw new IndexOutOfBoundsException("Value = " + other)
	}
}

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
		for (bearing <- BearingSet.All) {
			if (this.contains(bearing)) {
				if (needComma) {
					sb.append(",")
				}
				sb.append(bearing)
				needComma = true
			}
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
