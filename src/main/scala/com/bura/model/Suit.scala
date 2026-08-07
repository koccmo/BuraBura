package com.bura.model

sealed trait Suit {
  def symbol: String
}

object Suit {
  case object Hearts extends Suit { override val symbol: String = "♥" }

  case object Clubs extends Suit { override val symbol: String = "♣" }

  case object Spades extends Suit { override val symbol: String = "♠" }

  case object Diamonds extends Suit { override val symbol: String = "♦" }

  val ValuesList: List[Suit]       = List(Hearts, Clubs, Spades, Diamonds)
  val ValuesMap: Map[Suit, String] = ValuesList.map(x => x -> x.toString).toMap
}
