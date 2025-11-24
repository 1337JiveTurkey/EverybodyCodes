package grid

import scala.language.implicitConversions

/**
 * Directions as a case class
 *
 * @param dx Change in the x value to move in this direction
 * @param dy Change in the y value to move in this direction
 */
sealed abstract class Direction(ord: Int, val dx: Int, val dy: Int, val arrow: Char) {
	val cardinal: Boolean = (dx & dy) == 0
	val diagonal: Boolean = !cardinal
	val horizontal: Boolean = dx != 0 && dy == 0
	val vertical: Boolean = dx == 0 && dy != 0
	val bitMask: Int = 1 << ord

	/**
	 * Turn a relative bearing into an absolute direction.
	 * @param b The bearing.
	 * @return The direction that the bearing points in relative to this direction.
	 */
	def relative(b: Bearing): Direction = Direction.fromOrd((ord + b.dOrd) % 8)

	/**
	 * Turn a set of relative bearings into a set of absolute directions.
	 * @param bs The bearings relative to the current direction.
	 * @return The absolute directions.
	 */
	def relative(bs: BearingSet): DirectionSet = {
		val bits = bs.bearings & 0xff
		new DirectionSet((bits << ord | (bits >>> (8 - ord))) & 0xff)
	}

	def &(d: Direction): DirectionSet = DirectionSet(this) & d
	def |(d: Direction): DirectionSet = DirectionSet(this) | d
	def unary_~(): DirectionSet = ~DirectionSet(this)

	/**
	 * @return A single character representation of the direction as an arrow.
	 */
	implicit def toChar: Char = arrow
}

case object North      extends Direction(0,  0, -1, '\u2191')
case object Northeast  extends Direction(1,  1, -1, '\u2197')
case object East       extends Direction(2,  1,  0, '\u2192')
case object Southeast  extends Direction(3,  1,  1, '\u2198')
case object South      extends Direction(4,  0,  1, '\u2193')
case object Southwest  extends Direction(5, -1,  1, '\u2199')
case object West       extends Direction(6, -1,  0, '\u2190')
case object Northwest  extends Direction(7, -1, -1, '\u2196')

object Direction {
	def fromOrd(ord: Int): Direction = ord match {
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
