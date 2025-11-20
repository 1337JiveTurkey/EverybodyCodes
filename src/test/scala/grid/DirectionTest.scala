package grid

import org.junit.jupiter.api.{Assertions, Test}
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.{Arguments, MethodSource}


class DirectionTest {
	@Test def relativeBearings(): Unit = {
		Assertions.assertEquals(South, North.relative(Back))
		for (direction <- DirectionSet.All) {
			Assertions.assertEquals(direction, direction.relative(Fore))
		}
	}

	@Test def relativeBearingSets(): Unit = {
		// Taking every bearing off a direction should end up with every bearing
		for (direction <- DirectionSet.All) {
			Assertions.assertEquals(DirectionSet.All, direction.relative(BearingSet.All))
		}
		// Taking the cardinal bearings off the cardinal directions should end up with the cardinal directions
		for (direction <- DirectionSet.Cardinals) {
			Assertions.assertEquals(DirectionSet.Cardinals, direction.relative(BearingSet.Cardinals))
		}
		// Taking the diagonal bearings off the diagonal directions should end up with the cardinal directions
		for (direction <- DirectionSet.Diagonals) {
			Assertions.assertEquals(DirectionSet.Cardinals, direction.relative(BearingSet.Diagonals))
		}

	}

	@Test def day7FailingCode(): Unit = {
		val allButEast = DirectionSet.Cardinals - East
		val allButBack = BearingSet.Cardinals - Back
		Assertions.assertEquals(allButEast, West.relative(allButBack))
	}


	@ParameterizedTest
	@MethodSource(Array("directionBearingCombinations"))
	def day7FailingCodeExtension(d: Direction, b: Bearing): Unit = {
		val relativeD = d.relative(b)
		val allButB = BearingSet.All - b
		val allButRelativeD = DirectionSet.All - relativeD

		Assertions.assertEquals(allButRelativeD, d.relative(allButB))
	}
}

object DirectionTest {
	def directionBearingCombinations(): java.util.List[Arguments] = {
		val retVal = new java.util.ArrayList[Arguments]
		for (d <- DirectionSet.All) {
			for (b <- BearingSet.All) {
				retVal.add(Arguments.of(d, b))
			}
		}
		retVal
	}
}
