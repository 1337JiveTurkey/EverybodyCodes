package common

import scala.collection.mutable

/**
 * Counts how many objects of a type have been seen.
 *
 * @tparam T The type of the object being counted.
 */
class Counts[T] extends Iterable[(T, Long)] with (T => Long) {
	val internalMap: mutable.Map[T, Long] = new mutable.HashMap

	def addTo(t: T, count: Long): Long = {
		val currentCount = apply(t)
		val newTotal = currentCount + count
		update(t, newTotal)
		newTotal
	}

	def apply(t: T): Long = internalMap.getOrElse(t, 0)
	def update(t: T, count: Long): Unit = internalMap.put(t, count)

	def iterator: Iterator[(T, Long)] = internalMap.iterator

	def sum: Long = internalMap.values.sum
}
