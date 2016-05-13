package com.codurance.bowlingkata.partial_scoring

object BowlingPartialScoreCalculator {

	def scoreFor(rolls: String): Int = totalScore(rolls.split("").toList)

	private def totalScore(rolls: List[String], index: Int = 0, score: Int = 0): Int = {

		def numberOfPreviousStrikes(): Int = rolls.mkString.take(index).count(_ == 'X')

		lazy val STRIKE = ("X", strikeScore)
		lazy val SPARE = ("/", spareScore)
		lazy val MISS = "-"
		lazy val ROLL = "r"
		lazy val NONE = ""
		def roll(i: Int): Int = rollScoreAt(rolls.drop(i).headOption)._2
		def previousRoll(): Int = rollScoreAt(rolls.take(index).lastOption)._2

		def isSpare(i: Int): Boolean = rollScoreAt(rolls.drop(i).headOption)._1 == SPARE

		def spareScore() = 10 - previousRoll() + if_(index + numberOfPreviousStrikes < 19, roll(index + 1))

		def strikeScore() =
			10 + if_(index + numberOfPreviousStrikes < 18,
						roll(index + 2) + if_(!isSpare(index + 2), roll(index + 1)))

		def rollScoreAt(roll: Option[String]): (String, Int) =
			roll match {
				case Some("X") => (STRIKE._1, 10)
				case Some("/") => (SPARE._1, 10)
				case Some("-") => (MISS, 0)
				case Some(_)   => (ROLL, roll.get.toInt)
				case None => (NONE, 0)
			}

		rollScoreAt(rolls.drop(index).headOption) match {
			case (STRIKE._1, v) => totalScore(rolls, index + 1, score + strikeScore)
			case (SPARE._1, v) => totalScore(rolls, index + 1, score + spareScore)
			case (MISS, v) => totalScore(rolls, index + 1, score)
			case (NONE, v) => score
			case (ROLL, v) => totalScore(rolls, index + 1, score + v)
		}

	}

	private def if_(condition: Boolean, ifTrue: => Int): Int = if (condition) ifTrue else 0

}
