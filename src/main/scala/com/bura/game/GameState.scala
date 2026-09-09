package com.bura.game

import com.bura.model.{Attack, Deck, Defense, Player, Suit, WonTrick}


case class GameState(
  deck: Deck,
  players: Vector[Player],
  attacker: Int,
  defender: Int,
  trump: Suit,
  round: Round = Round.WaitingAttack
) {

  //Players
  def attackingPlayer: Player = players(attacker)
  def defendingPlayer: Player = players(defender)

  //Update players
  private def updatePlayer(index: Int, player: Player): GameState =
    copy(
      players = players.updated(index, player)
    )

  private def updateAttacker(player: Player): GameState = updatePlayer(attacker, player)

  private def updateDefender(player: Player): GameState = updatePlayer(defender, player)

  def modifyPlayer(index: Int)(f: Player => Player): GameState = updatePlayer(index,f(players(index)))
  def modifyAttacker(f: Player => Player): GameState           = updateAttacker(f(attackingPlayer))
  def modifyDefender(f: Player => Player): GameState           = updateDefender(f(defendingPlayer))

  //Round
  def withAttack(attack: Attack): GameState =
    copy(round = Round.WaitingDefense(attack))

  def defenderWon(attack: Attack, defense: Defense): GameState = {
    val trick: WonTrick.Open = WonTrick.Open(attack.cards ++ defense.cards)

    copy(round = Round.DefenderWon(trick))
  }

  def attackWon(attack: Attack, defense: Defense): GameState = {
    val trick: WonTrick.Open = WonTrick.Open(attack.cards ++ defense.cards)

    copy(round = Round.AttackerWon(trick))
  }

  def attackerWonHidden(attack: Attack, defense: Defense): GameState = {
    val openTrick: WonTrick.Open = WonTrick.Open(attack.cards)
    val hiddenTrick: WonTrick.Hidden = WonTrick.Hidden(defense)

    copy(round = Round.AttackerWonHidden(openTrick, hiddenTrick))
  }

  //Next round
  def nextRound(winnerIndex: Int): GameState = {
    val nextDefender = (winnerIndex + 1) % players.size

    copy(attacker = winnerIndex, defender = nextDefender, round = Round.WaitingAttack)
  }
  //Draw cards
  def refillHands: GameState = {
    val (updatedAttacker, deckAfterAttacker) = deck.drawTo(attackingPlayer)
    val stateAfterAttacker = copy(deck = deckAfterAttacker).updateAttacker(updatedAttacker)
    val (updatedDefender, deckAfterDefender) = stateAfterAttacker.deck.drawTo(stateAfterAttacker.defendingPlayer)

    stateAfterAttacker.copy(deck = deckAfterDefender).updateDefender(updatedDefender)
  }

}
