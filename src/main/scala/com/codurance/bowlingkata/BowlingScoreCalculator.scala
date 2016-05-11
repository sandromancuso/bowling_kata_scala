package com.codurance.bowlingkata

object BowlingScoreCalculator {

	def scoreFor(rolls: String): Int =
		totalScore(rolls.split("").toList)

	private def totalScore(rolls: List[String], index: Int = 0, score: Int = 0): Int =
		rolls.drop(index) match {
			case List(STRIKE._1, _*) => totalScore(rolls, index + 1, score + strike(rolls, index))
			case List(SPARE, _*) => totalScore(rolls, index + 1, score + spare(rolls, index))
			case List(MISS._1, _*) => totalScore(rolls, index + 1, score)
			case List(s, _*) => totalScore(rolls, index + 1, score + s.toInt)
			case List() => score
		}

	private def strike(rolls: List[String], index: Int): Int =
		if (numberOfPreviousStrikes(rolls, index) + index < TENTH_FRAME_FIRST_ROLL)
	 		 10 + bonusRoll(rolls, index + 1) + bonusRoll(rolls, index + 2)
		else 10

	private def numberOfPreviousStrikes(rolls: List[String], index: Int): Int = rolls.mkString.take(index).count(_ == STRIKE._1.head)

	private def spare(rolls: List[String], index: Int): Int =
		if (index < TENTH_FRAME_SECOND_ROLL)
			 spareValue(rolls, index) + bonusRoll(rolls, index + 1)
		else spareValue(rolls, index)

	private def spareValue(rolls: List[String], index: Int): Int = 10 - rolls(index - 1).toInt

	private def bonusRoll(rolls: List[String], index: Int): Int =
		value(rolls.lift(index).getOrElse("0")).toInt

	private def value(s: String): String =
		s match {
			case MISS._1 => MISS._2
			case STRIKE._1 => STRIKE._2
			case _ => s
		}

	val MISS = ("-", "0")
	val STRIKE = ("X", "10")
	val SPARE = "/"
	val MAX_ROLLS = 21
	val TENTH_FRAME_FIRST_ROLL = MAX_ROLLS - 3
	val TENTH_FRAME_SECOND_ROLL = MAX_ROLLS - 2
}
