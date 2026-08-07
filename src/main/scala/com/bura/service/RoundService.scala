package com.bura.service

import com.bura.model.{Attack, Defense, GameState, Hidden, Open, Player}
import com.bura.rules.Rules

object RoundService {

  def resolve(state: GameState): GameState =
    currentPlay(state) match {
      case Some((attack, defense)) =>
        defense.mode match {
          case Open   =>
            if (Rules.beatsAttack(attack, defense, state.trump)) successfulDefense(state, attack, defense)
            else faultDefense(state, attack, defense)
          case Hidden => hiddenDefense(state, attack, defense)
        }
      case None                    => state
    }

  private def currentPlay(state: GameState): Option[(Attack, Defense)] =
    for {
      attack  <- state.attack
      defense <- state.defense
    } yield (attack, defense)

  private def successfulDefense(
    state: GameState,
    attack: Attack,
    defense: Defense
  ): GameState =
    state
      .modifyDefender(_.addTrick(Trick(attack, defense)))
      .refillHands
      .swapPlayers
      .clearTable

  private def faultDefense(
    state: GameState,
    attack: Attack,
    defense: Defense
  ): GameState = ???

  private def hiddenDefense(
    state: GameState,
    attack: Attack,
    defense: Defense
  ): GameState =
    state
      .modifyAttacker(_.addTrick(Trick(attack, null)))
      .modifyAttacker(_.addHiddenTrick(HiddenTrick(defense.cards)))
      .refillHands
      .clearTable
}
