package grid

import org.junit.{Assert, Test}

class DirectionTest() {
	@Test def relativeBearings(): Unit = {
		Assert.assertEquals(South, North.relative(Back))
		for (direction <- DirectionSet.All) {
			Assert.assertEquals("Directions should be same as their relative(Fore)", direction, direction.relative(Fore))
		}
	}

	@Test def relativeBearingSets(): Unit = {
		// Taking every bearing off a direction should end up with every bearing
		for (direction <- DirectionSet.All) {
			Assert.assertEquals(DirectionSet.All, direction.relative(BearingSet.All))
		}
		// Taking the cardinal bearings off the cardinal directions should end up with the cardinal directions
		for (direction <- DirectionSet.Cardinals) {
			Assert.assertEquals(DirectionSet.Cardinals, direction.relative(BearingSet.Cardinals))
		}
		// Taking the diagonal bearings off the diagonal directions should end up with the cardinal directions
		for (direction <- DirectionSet.Diagonals) {
			Assert.assertEquals(DirectionSet.Cardinals, direction.relative(BearingSet.Diagonals))
		}

		val allButEast = DirectionSet(North, South, West)
		val allButBack = BearingSet(Fore, Left, Right)
		Assert.assertEquals(allButEast, West.relative(allButBack))
	}
}
