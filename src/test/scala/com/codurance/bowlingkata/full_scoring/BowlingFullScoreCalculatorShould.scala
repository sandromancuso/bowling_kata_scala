package com.codurance.bowlingkata.full_scoring

import com.codurance.UnitSpec
import com.codurance.bowlingkata.full_scoring.BowlingFullScoreCalculator.scoreFor
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BowlingFullScoreCalculatorShould extends UnitSpec {

	"calculate scores with no strikes or spares" in {
		scoreFor("11111111112222222222") should be (30)
	}

	"calculate scores containing a miss" in {
		scoreFor("--------------------") should be (0)
		scoreFor("1-1----------------1") should be (3)
		scoreFor("9-9-9-9-9-9-9-9-9-9-") should be (90)
	}

	"calculate scores containing spares" in {
		scoreFor("5/11------------3/11") should be (26)
		scoreFor("5/5/5/5/5/5/5/5/5/5/5") should be (150)
	}

	"calculate scores containing strikes" in {
		scoreFor("XXXXXXXXXXXX") should be(300)
		scoreFor("XXXXXXXXXX12") should be(274)
		scoreFor("1/35XXX458/X3/23") should be(160)
		scoreFor("1/35XXX458/X3/XX6") should be(189)
	}
}
