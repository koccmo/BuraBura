package com.bura.ai

import com.bura.model.{Card, Suit}

case class HandAnalysis (
  cards: List[Card],
  trumpCards: List[Card],
  nonTrumpCards: List[Card],
  trumpPoints: Int,
  nonTrumpPoints: Int,
  suitsAmount: Int
                        )

object HandAnalysis {

  def from(hand: List[Card], trump: Suit): HandAnalysis = {
    val sortedCards = hand.sortBy(_.rank.strength)
    val trumpCards : List[Card] = sortedCards.filter(_.suit == trump)
    val nonTrumpCards: List[Card] = sortedCards.filter(_.suit != trump)

    HandAnalysis(
      cards = sortedCards,
      trumpCards = trumpCards,
      nonTrumpCards = nonTrumpCards,
      trumpPoints = trumpCards.map(_.points).sum,
      nonTrumpPoints = nonTrumpCards.map(_.points).sum,
      suitsAmount = sortedCards.map(_.suit).distinct.size
    )
  }
}
