package grid

import org.junit.{Assert, Test}

class DirectionTest() {
	@Test def relativeBearings(): Unit = {
		Assert.assertEquals(South, North.relative(Back))
		for (direction <- DirectionSet.All) {
			Assert.assertEquals(DirectionSet.All, direction.relative(BearingSet.All))
		}
	}
}
