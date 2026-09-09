package com.bura.service

import com.bura.game.{GameResult, GameState}
import com.bura.model.Player

object GameResultService {

  def gameResult(state: GameState): Either[String, GameResult] = {

    for {
      human <- state.players.collectFirst {
          case human: Player.Human => human }.toRight("Human player not found")

      robot <- state.players.collectFirst {
          case robot: Player.Robot => robot }.toRight("Robot player not found")

    } yield {

      val humanPoints = human.totalPoints
      val robotPoints = robot.totalPoints

      if (humanPoints > robotPoints) GameResult.HumanWon(humanPoints, robotPoints)
      else GameResult.RobotWon(humanPoints, robotPoints)
    }
  }

}
