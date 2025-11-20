package grid

sealed class EdgeBehavior(val wrapX: Boolean, val wrapY: Boolean)

case object WrapNeither extends EdgeBehavior(false, false)
case object WrapX extends EdgeBehavior(true, false)
case object WrapY extends EdgeBehavior(false, true)
case object WrapBoth extends EdgeBehavior(true, true)
