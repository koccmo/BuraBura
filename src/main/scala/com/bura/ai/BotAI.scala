package com.bura.ai

import com.bura.game.GameState
import com.bura.model.{Attack, Card, Defense, Player, Suit}
import com.bura.rules.Rules

object BotAi {
  def chooseAttack(state: GameState, robot: Player.Robot): Option[Attack] = {
    BotAttack.choose(state, robot)
  }

  def chooseDefense(state: GameState, robot: Player.Robot, attack: Attack): Option[Defense] = {
    BotDefense.choose(state, robot, attack)
  }

  def chooseSurrenderDefense(state: GameState, robot: Player.Robot, attack: Attack): Option[Defense] =
    BotDefense.chooseSurrenderDefense(state, robot, attack)

  def shouldDeclare31(robot: Player.Robot): Boolean = {

    val visiblePoints = robot.trickPoints
    val hiddenCardsAmount = robot.hiddenTricks.flatMap(_.reveal).size

    if (visiblePoints >= 31) true
    else if (visiblePoints >= 25 && hiddenCardsAmount >= 1) true
    else if (visiblePoints >= 20 && hiddenCardsAmount >= 2) true
    else false
  }
}
