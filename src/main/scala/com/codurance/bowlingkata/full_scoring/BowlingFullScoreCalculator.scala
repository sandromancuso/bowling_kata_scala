package com.codurance.bowlingkata.full_scoring

object BowlingFullScoreCalculator {

	def scoreFor(rolls: String): Int = totalScore(rolls.split("").toList)

	private def totalScore(rolls: List[String], index: Int = 0, score: Int = 0): Int = {
		lazy val MISS  = "-"
		lazy val SPARE = ("/", () => 10 - rollScoreAt(index - 1) + if_(index < 19, rollScoreAt(index + 1)))
		lazy val STRIKE = ("X", () => 10 + if_(index + numberOfPreviousStrikes() < 18,
												rollScoreAt(index + 1) + rollScoreAt(index + 2)))

		def numberOfPreviousStrikes(): Int = rolls.mkString.take(index).count(_ == 'X')

		def rollScoreAt(index: Int): Int =
			rolls(index) match {
				case STRIKE._1 => 10
				case SPARE._1  => 10 - rolls(index - 1).toInt
				case MISS      => 0
				case pins      => pins.toInt
			}

		rolls.drop(index) match {
			case STRIKE._1 :: _ => totalScore(rolls, index + 1, score + STRIKE._2())
			case SPARE._1 :: _  => totalScore(rolls, index + 1, score + SPARE._2())
			case MISS :: _      => totalScore(rolls, index + 1, score)
			case n :: _         => totalScore(rolls, index + 1, score + n.toInt)
			case List()         => score
		}
	}

	private def if_(condition: Boolean, ifTrue: => Int): Int = if (condition) ifTrue else 0
}