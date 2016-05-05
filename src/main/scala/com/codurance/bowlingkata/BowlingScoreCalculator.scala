package com.codurance.bowlingkata

object BowlingScoreCalculator {

	def scoreFor(rolls: String): Int =
		calculateScore(toFrames(toPins(rolls.split(""))))

	def toPins(rolls: Array[String], pins: Array[Int] = Array()): Array[Int] =
		rolls match {
			case Array("-", _*) => toPins(rolls.tail, pins :+ 0)
			case Array("/", _*) => toPins(rolls.tail, pins :+ 10 - pins(pins.size - 1))
			case Array("X", _*) => toPins(rolls.tail, pins :+ 10)
			case Array(s, _*) => toPins(rolls.tail, pins :+ s.toInt)
			case Array() => pins
		}

	def toFrames(pins: Array[Int], frames: Array[Frame] = Array()): Array[Frame] =
		pins match {
			case Array() => frames
			case Array(x, y, z) => toFrames(pins.drop(3), frames :+ Frame(x, Some(y), Some(z)))
			case Array(10, _*) => toFrames(pins.drop(1), frames :+ Frame(10))
			case Array(x, y, _*) => toFrames(pins.drop(2), frames :+ Frame(x, Some(y)))
		}

	def calculateScore(frames: Array[Frame], score: Int = 0): Int =
		if (!frames.isEmpty) {
			val currentFrame = frames.head
			currentFrame.frameType() match {
				case SPARE => calculateScore(frames.tail, score + currentFrame.pins() + nextFrame(frames).first)
				case STRIKE => {
					calculateScore(frames.tail, score + strikeScore(currentFrame, frames.tail))
				}
				case _ => calculateScore(frames.tail, score + currentFrame.pins())
			}
		} else {
			score
		}

	def strikeScore(currentFrame: Frame, nextFrames: Array[Frame]): Int =
		nextFrames match {
			case Array() => currentFrame.pins()
			case Array(next, _*) =>
				if (next.frameType() == STRIKE)
					currentFrame.pins() + next.pins() + nextFrame(nextFrames).first
				else currentFrame.pins() + next.first + next.second.getOrElse(0)
		}

	def nextFrame(frames: Array[Frame]): Frame =
		frames.tail.headOption.getOrElse(Frame(0))

}

case class Frame(first: Int, second: Option[Int] = None, third: Option[Int] = None) {
	def pins(): Int = first + second.getOrElse(0) + third.getOrElse(0)

	def frameType(): FrameType =
		if (first == 10 && third.isEmpty) {
			STRIKE
		} else if (first + second.getOrElse(0) == 10) {
			SPARE
		} else {
			NORMAL
		}

	override def toString: String = s"${first} - ${second.getOrElse(0)} - ${third.getOrElse(0)}"
}

sealed abstract class FrameType
case object STRIKE extends FrameType
case object SPARE  extends FrameType
case object NORMAL extends FrameType



