package com.bura.model

import com.bura.rules.{BuraRules, Rules}

case class Player(
  name: String,
  hand: List[Card],
  tricks: List[WonTrick.Open] = Nil,
  hiddenTricks: List[WonTrick.Hidden] = Nil,
  wins: Int = 0
) {

  // Work with hand
  def addCards(cards: List[Card]): Player =
    copy(hand = hand ++ cards)

  def removeCards(cards: List[Card]): Player = {
    require(cards.forall(hand.contains), "Player does not have these cards")
    copy(hand = hand.diff(cards))
  }

  // Game

  def playAttack(attack: Attack): Player =
    removeCards(attack.cards)

  def playDefense(defense: Defense): Player =
    removeCards(defense.cards)

  // Tricks

  def addTrick(trick: WonTrick.Open): Player =
    copy(tricks = tricks :+ trick)

  def addHiddenTrick(hiddenTrick: WonTrick.Hidden): Player =
    copy(hiddenTricks = hiddenTricks :+ hiddenTrick)

  // Info

  lazy val trickPoints: Int = tricks.map(_.points).sum


  def totalPoint: Int = {
    val hidden = hiddenTricks.map(_.reveal.points).sum

    trickPoints + hidden
  }

  def hasCards(cards: List[Card]): Boolean =
    cards.forall(hand.contains)

  def canAttack: Boolean =
    hand.nonEmpty

  def needCards: Int =
    BuraRules.HandSize - hand.size

  def addWins: Player =
    copy(wins = wins + 1)
}
