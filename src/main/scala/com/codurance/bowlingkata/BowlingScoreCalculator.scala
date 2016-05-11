package com.codurance.bowlingkata

object BowlingScoreCalculator {

	def scoreFor(rolls: String): Int =
		calculateScore(toPins(rolls.split("")))

	private def toPins(rolls: Array[String], pins: Array[Int] = Array()): Array[Int] =
		rolls match {
			case Array() => pins
			case Array("-", _*) => toPins(rolls.tail, pins :+ 0)
			case Array("/", _*) => toPins(rolls.tail, pins :+ 10 - pins.last)
			case Array(c, _*) => toPins(rolls.tail, pins :+ c.toInt)
		}

	private def calculateScore(pins: Array[Int]): Int = {
		var score = 0
		for (i <- pins.indices) {
			score += pins(i)
		     if (i % 2 == 0) {
			    if (pins(i - 1) + pins(i) == 10) {
					score += (10 + pins.applyOrElse(i + 1, _ => 0))
				}
			}
		}
		score
	}

}
