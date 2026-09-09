package com.bura.model

sealed trait WonTrick {
  def count: Int
}

object WonTrick {

  case class Open(cards: List[Card]) extends WonTrick {

    lazy val points: Int = cards.map(_.points).sum

    override def count: Int = cards.size
  }

  case class Hidden(defense: Defense) extends WonTrick {

    override val count: Int = defense.cards.size

    def reveal: List[Card] = defense.cards

    def revealPoints: Int = defense.cards.map(_.points).sum
  }
}
