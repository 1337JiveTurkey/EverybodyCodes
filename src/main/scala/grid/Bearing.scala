package grid

/**
 * Directions relative to a given direction given as a case class.
 *
 * @param dOrd The ordinal of the object, selected for easy math.
 */
sealed abstract class Bearing(val dOrd: Int) {
	val bitMask: Int = 1 << dOrd
}

case object Fore       extends Bearing(0)
case object ForeRight  extends Bearing(1)
case object Right      extends Bearing(2)
case object BackRight  extends Bearing(3)
case object Back       extends Bearing(4)
case object BackLeft   extends Bearing(5)
case object Left       extends Bearing(6)
case object ForeLeft   extends Bearing(7)

object Bearing {
	def fromOrd(ord: Int): Bearing = ord match {
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
