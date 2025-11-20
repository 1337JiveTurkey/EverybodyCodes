package grid

import scala.collection.mutable

/**
 * An individual cell on a grid with various operations to access its neighbors.
 *
 * @tparam T The type of data stored within the cell.
 */
trait Cell[T] extends Point {
	def value: T

	def value_=(t: T): Unit

	/**
	 * Gets the cell in the given direction
	 *
	 * @param d The direction to get the cell from.
	 * @return Some(cell) if there is a cell in that direction or else None
	 */
	def get(d: Direction): Option[Cell[T]]

	/**
	 *
	 * @return True if this is on any edge of the grid
	 */
	def onEdge: Boolean

	/**
	 *
	 * @return The set of directions that are still on the grid
	 */
	def validDirections: DirectionSet

	/**
	 * Gets all the cells in a given direction to the edge of the grid.
	 * DOES NOT INCLUDE THE ORIGINAL CELL.
	 *
	 * @param d The direction to look for cells
	 * @return All cells in the direction given, from nearest to furthest
	 */
	def ray(d: Direction): LazyList[Cell[T]] = {
		val cell = get(d)
		if (cell.isDefined) {
			cell.get #:: cell.get.ray(d)
		} else {
			LazyList.empty
		}
	}

	/**
	 * Gets all the neighbors in the given directions.
	 *
	 * @param dirs The DirectionSet to get the cells in the direction of
	 * @return The cells adjacent to the current cell.
	 */
	def neighbors(dirs: DirectionSet): List[Cell[T]] = {
		var cells: List[Cell[T]] = List.empty
		(validDirections & dirs).foreach(dir => {
			cells = get(dir).get :: cells
		})
		cells
	}

	def neighbors: List[Cell[T]] = neighbors(DirectionSet.All)

	def floodFill(dirs: DirectionSet)(membership: Cell[T] => Boolean): List[Cell[T]] = {
		val visited = mutable.Set(this)
		val queue = mutable.Queue(this)

		while (queue.nonEmpty) {
			val cell = queue.dequeue()
			for (neighbor <- cell.neighbors(dirs)) {
				if (membership(neighbor) && !visited(neighbor)) {
					visited.add(neighbor)
					queue.enqueue(neighbor)
				}
			}
		}
		visited.toList
	}
}

object Cell {
	def unapply[T](c: Cell[T]): Option[T] = {
		Some(c.value)
	}
}