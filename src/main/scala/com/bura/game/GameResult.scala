package com.bura.game

sealed trait GameResult {
  def humanPoints: Int
  def robotPoints: Int
}

object GameResult {
  case class HumanWon(humanPoints: Int, robotPoints: Int) extends GameResult
  case class RobotWon(humanPoints: Int, robotPoints: Int) extends GameResult
  case class Draw(humanPoints: Int, robotPoints: Int) extends GameResult
}