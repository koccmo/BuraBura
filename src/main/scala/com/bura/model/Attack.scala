package com.bura.model

case class Attack(cards: List[Card]) {

  require(cards.nonEmpty && cards.size <= 3, "Attack must contain 1-3 cards")

  require(sameSuit, "All attack cards must have the same suit")

  private def sameSuit: Boolean =
    cards
      .map(_.suit)
      .distinct
      .size == 1

  def size: Int = cards.size

  def suit: Suit = cards.head.suit
}
