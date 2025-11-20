package grid

import org.junit.{Assert, Test}

class DirectionTest {
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

	}

	@Test def day7FailingCode(): Unit = {
		val allButEast = DirectionSet.Cardinals - East
		val allButBack = BearingSet.Cardinals - Back
		Assert.assertEquals(allButEast, West.relative(allButBack))
	}

	@Test def day7FailingCodeExtension(): Unit = {
		for (d <- DirectionSet.All) {
			val allButD = DirectionSet.All - d
			for (b <- BearingSet.All) {
				val relativeD = d.relative(b)
				val allButB = BearingSet.All - b
				val allButRelativeD = DirectionSet.All - relativeD

				if (allButRelativeD == d.relative(allButB)) {
					println(s"Success for $d and $b")
				} else {
					println(s"Failure for $d and $b")
				}
			}
		}
	}

}
