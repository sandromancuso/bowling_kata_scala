package com.codurance.bowlingkata.full_scoring

object BowlingScoreCalculator {

	def scoreFor(rolls: String): Int = totalScore(rolls.split("").toList)

	def totalScore(rolls: List[String], index: Int = 0, score: Int = 0): Int = {

		def numberOfPreviousStrikes(): Int = rolls.mkString.take(index).count(_ == STRIKE.charAt(0))
		def ball(index: Int): Int = valueOf(rolls(index), index)
		def strike(): Int = 10 + if_(index + numberOfPreviousStrikes < 18, ball(index + 1) + ball(index + 2))
		def spare(): Int = 10 - valueOf(rolls(index - 1), index - 1) + if_(index < 19, ball(index + 1))

		def valueOf(s: String, index: Int): Int =
			s match {
				case STRIKE => 10
				case SPARE => 10 - rolls(index - 1).toInt
				case MISS => 0
				case _ => s.toInt
			}

		rolls.drop(index) match {
			case List(STRIKE, _*) => totalScore(rolls, index + 1, score + strike)
			case List(SPARE, _*)  => totalScore(rolls, index + 1, score + spare)
			case List(MISS, _*)   => totalScore(rolls, index + 1, score)
			case List(n, _*)      => totalScore(rolls, index + 1, score + n.toInt)
			case List() => score
		}
	}

	val STRIKE = "X"
	val SPARE = "/"
	val MISS  = "-"

	def if_(condition: Boolean, ifTrue: => Int): Int = if (condition) ifTrue else 0
}
