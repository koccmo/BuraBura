package com.bura.model

final case class Card(rank: Rank, suit: Suit) {

  def points: Int = rank.points

  def strength: Int = rank.strength

  override def toString: String = s"${rank.shortName}${suit.symbol}"
}
