package grid

/**
 * A point in 2D integer space with two coordinates, typically called x and y.
 * Well, always called x and y as far as this is concerned.
 */
trait Point {
	def x: Int
	def y: Int

	def euclidianSquaredTo(p: Point): Int = (x - p.x) * (x - p.x) + (y - p.y) * (y - p.y)
	/**
	 * Number of blocks that a taxicab needs to move to get from one block to another.
	 *
	 * @param p The point to measure to
	 * @return The Manhattan distance between two points
	 */
	def manhattanTo(p: Point): Int = Math.abs(x - p.x) + Math.abs(y - p.y)

	/**
	 * Number of moves it takes a chessboard king to move from one point to another.
	 *
	 * @param p The point to measure to
	 * @return The Chebyshev distance between two points
	 */
	def chebyshevTo(p: Point): Int = Math.max(Math.abs(x - p.x), Math.abs(y - p.y))
}

object Point {
	private case class PointAt(x: Int, y: Int) extends Point

	def apply(x: Int, y: Int): Point = PointAt(x, y)
	def unapply(p: Point): Option[(Int, Int)] = {
		Some((p.x, p.y))
	}
}
