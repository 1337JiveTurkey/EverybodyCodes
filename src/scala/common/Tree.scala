package common

import scala.collection.mutable

/**
 * A mutable Tree data structure.
 *
 * @tparam K The type of the tree node identifier.
 * @tparam V The type of the tree node value.
 */
class Tree[K, V] {
	private[this] var root: Option[TreeNodeImpl] = None
	private[this] val index = mutable.HashMap.empty[K, TreeNodeImpl]

	def get(k: K): TreeNode[K, V] = {
		if (index.contains(k)) {
			index(k)
		} else {
			val retVal = TreeNodeImpl(k)
			index(k) = retVal
			retVal
		}
	}

	def breadthFirstSearch(): Iterator[TreeNode[K, V]] = {
		if (root.isEmpty) {
			Iterator.empty
		}
		new Iterator[TreeNode[K, V]] {
			val queue = new mutable.Queue[TreeNodeImpl]()
			queue.enqueue(root.get)
			def hasNext: Boolean = queue.nonEmpty

			def next(): TreeNode[K, V] = {
				val retVal = queue.dequeue()
				queue.enqueueAll(retVal.children.asInstanceOf[List[TreeNodeImpl]])
				retVal
			}
		}
	}

	def depthFirstSearch(): Iterator[TreeNode[K, V]] = {
		if (root.isEmpty) {
			Iterator.empty
		}
		new Iterator[TreeNode[K, V]] {
			val stack = new mutable.Stack[TreeNodeImpl]()
			stack.push(root.get)
			def hasNext: Boolean = stack.nonEmpty

			def next(): TreeNode[K, V] = {
				val retVal = stack.pop()
				stack.pushAll(retVal.children.asInstanceOf[List[TreeNodeImpl]])
				retVal
			}
		}
	}

	private case class TreeNodeImpl(id: K) extends TreeNode[K, V] {
		var value: V = null.asInstanceOf[V]
		var parentID: Option[K] = None
		var childIDs: List[K] = List.empty
		def parent: Option[TreeNode[K, V]] = parentID.map(index)
		def children: List[TreeNode[K, V]] = childIDs.map(index)

		def setRoot(): Unit = {
			root = Some(this)
		}
		def addChild(child: K): Unit = {
			childIDs = child :: childIDs
			val childNode = get(child)
			childNode.asInstanceOf[TreeNodeImpl].parentID = Some(this.id)
		}
	}
}

object Tree {

}

trait TreeNode[K, V] {
	def id: K
	def value: V
	def value_=(newValue: V): Unit
	def parent: Option[TreeNode[K, V]]
	def children: List[TreeNode[K, V]]

	def setRoot(): Unit
	def addChild(k: K): Unit

	def ancestors: List[TreeNode[K, V]] = {
		var path: List[TreeNode[K, V]] = List.empty
		var node: Option[TreeNode[K, V]] = Some(this)
		while (node.isDefined) {
			val realNode = node.get
			path = realNode :: path
			node = realNode.parent
		}
		path
	}
}
