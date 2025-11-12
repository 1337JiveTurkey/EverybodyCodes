package tyool2024

import common.Tree

object Day6 extends Main {
	def main(args: Array[String]): Unit = {
		star3()
	}

	def star1(): Unit = {
		val lines = fileLines("Day6Star1.txt")

		val tree = new Tree[String, Boolean]()
		for (line <- lines) {
			val parts = line.split(':')
			val nodeID = parts(0)
			val childIDs = parts(1).split(',')
			val node = tree.get(nodeID)
			for (childID <- childIDs) {
				if (childID == "@") {
					node.value = true
				} else {
					node.addChild(childID)
				}
			}
		}
		tree.get("RR").setRoot()
		for (node <- tree.breadthFirstSearch()) {
			if (node.value) {
				println(node.ancestors.map(_.id).mkString + '@')
			}
		}
	}

	def star2(): Unit = {
		val lines = fileLines("Day6Star2.txt")

		val tree = new Tree[String, Boolean]()
		for (line <- lines) {
			val parts = line.split(':')
			val nodeID = parts(0)
			val childIDs = parts(1).split(',')
			val node = tree.get(nodeID)
			for (childID <- childIDs) {
				if (childID == "@") {
					node.value = true
				} else {
					node.addChild(childID)
				}
			}
		}
		tree.get("RR").setRoot()
		for (node <- tree.breadthFirstSearch()) {
			if (node.value) {
				println(node.ancestors.map(_.id.charAt(0)).mkString + '@')
			}
		}
	}

	def star3(): Unit = {
		val lines = fileLines("Day6Star3.txt")

		val tree = new Tree[String, Boolean]()
		for (line <- lines) {
			val parts = line.split(':')
			val nodeID = parts(0)
			val childIDs = parts(1).split(',')
			val node = tree.get(nodeID)
			for (childID <- childIDs) {
				if (childID == "@") {
					node.value = true
				} else {
					node.addChild(childID)
				}
			}
		}
		tree.get("RR").setRoot()
		for (node <- tree.breadthFirstSearch()) {
			if (node.value) {
				println(node.ancestors.map(_.id.charAt(0)).mkString + '@')
			}
		}
	}
}
