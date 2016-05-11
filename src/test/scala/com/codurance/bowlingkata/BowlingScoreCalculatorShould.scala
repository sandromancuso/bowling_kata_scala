package com.codurance.bowlingkata

import com.codurance.UnitSpec
import com.codurance.bowlingkata.BowlingScoreCalculator.scoreFor
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BowlingScoreCalculatorShould extends UnitSpec {

	"calculate scores with no strikes or spares" in {
		scoreFor("13") should be (4)
		scoreFor("135213") should be (15)
	}

	"calculate scores containing a miss" in {
		scoreFor("1-5-") should be (6)
		scoreFor("9-9-9-9-9-9-9-9-9-9-") should be (90)
	}

	"calculate scores containing spares" in {
//		scoreFor("1/") should be (10)
		scoreFor("1/--") should be (10)
		scoreFor("1/-5") should be (15)
		scoreFor("1/35") should be (21)
//		scoreFor("1/3/23") should be (30)
//		scoreFor("5/5/5/5/5/5/5/5/5/5/5") should be (150)
	}

	"calculate scores containing strikes" in {
//		scoreFor("X") should be (10)
//		scoreFor("X--") should be (10)
//		scoreFor("X--51") should be (16)
//		scoreFor("X51") should be (16)
//		scoreFor("XXXXXXXXXXXX") should be(300)
//		scoreFor("XXXXXXXXXX12") should be(274)
	}
}
