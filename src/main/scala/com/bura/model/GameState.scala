package com.bura.model

case class GameState(
  deck: Deck,
  players: Vector[Player],
  attacker: Int,
  defender: Int,
  trump: Suit,
  attack: Option[Attack] = None,
  defense: Option[Defense] = None
) {
  def attackingPlayer: Player                                     = players(attacker)
  def defendingPlayer: Player                                     = players(defender)
  private def updatePlayer(index: Int, player: Player): GameState =
    copy(
      players = players.updated(index, player)
    )

  private def updateAttacker(player: Player): GameState = updatePlayer(attacker, player)

  private def updateDefender(player: Player): GameState = updatePlayer(defender, player)

  def modifyAttacker(f: Player => Player): GameState = updateAttacker(f(attackingPlayer))
  def modifyDefender(f: Player => Player): GameState = updateDefender(f(defendingPlayer))

//  def swapPlayers: GameState =
//    copy(attacker = defender, defender = attacker)
  def nextRound(winnerIndex: Int): GameState = ???

  def clearTable: GameState =
    copy(attack = None, defense = None)

  def withAttack(attack: Attack): GameState =
    copy(attack = Some(attack))

  def withDefense(defense: Defense): GameState =
    copy(defense = Some(defense))

  def refillHands: GameState = {
    val (updatedAttacker, deckAfterAttacker) = deck.drawTo(attackingPlayer)
    val stateAfterAttacker                   = copy(deck = deckAfterAttacker).updateAttacker(updatedAttacker)
    val (updatedDefender, deckAfterDefender) = deckAfterAttacker.drawTo(stateAfterAttacker.defendingPlayer)

    stateAfterAttacker.copy(deck = deckAfterDefender).updateDefender(updatedDefender)
  }
}
