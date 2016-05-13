package com.codurance.bowlingkata.partial_scoring

object BowlingScoreCalculator {

	def scoreFor(rolls: String): Int =
		totalScore(rolls.split("").toList)

	private def valueOf(roll: Option[String]): (String, Int) =
		roll match {
			case Some("X") => (STRIKE, 10)
			case Some("/") => (SPARE, 10)
			case Some("-") => (MISS, 0)
			case Some(_)   => (ROLL, roll.get.toInt)
			case None => (NONE, 0)
		}

	private def totalScore(rolls: List[String], index: Int = 0, score: Int = 0): Int = {

		def numberOfPreviousStrikes(): Int = rolls.mkString.take(index).count(_ == STRIKE.charAt(0))
		def roll(i: Int): Int = valueOf(rolls.drop(i).headOption)._2
		def previousRoll(): Int = valueOf(rolls.take(index).lastOption)._2

		def isSpare(i: Int): Boolean = valueOf(rolls.drop(i).headOption)._1 == SPARE

		def spare() = if (index + numberOfPreviousStrikes < TENTH_FRAME_SECOND_ROLL )
					       10 - previousRoll() + roll(index + 1)
	                  else 10 - previousRoll()

		def strike() = {
			var score = 10
			if (index + numberOfPreviousStrikes < TENTH_FRAME_FIRST_ROLL)
				if (isSpare(index + 2))
					 score += roll(index + 2)
				else score += roll(index + 1) + roll(index + 2)
			score
		}

		valueOf(rolls.drop(index).headOption) match {
			case (STRIKE, v) => totalScore(rolls, index + 1, score + strike)
			case (SPARE, v) => totalScore(rolls, index + 1, score + spare)
			case (MISS, v) => totalScore(rolls, index + 1, score)
			case (NONE, v) => score
			case (ROLL, v) => totalScore(rolls, index + 1, score + v)
		}
	}

	val MISS = "-"
	val STRIKE = "X"
	val SPARE = "/"
	val ROLL = "r"
	val NONE = ""
	val MAX_ROLLS = 21
	val TENTH_FRAME_FIRST_ROLL = MAX_ROLLS - 3
	val TENTH_FRAME_SECOND_ROLL = MAX_ROLLS - 2
}
