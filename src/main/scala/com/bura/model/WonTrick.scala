package com.bura.model

sealed trait WonTrick {

  def attack: Attack
  def defense: Defense

  lazy val cards: List[Card] = attack.cards ++ defense.cards
}

object WonTrick {

  case class Open(attack: Attack, defense: Defense) extends WonTrick {

    lazy val points: Int = cards.map(_.points).sum
  }

  case class Hidden(attack: Attack, defense: Defense) extends WonTrick {

    def hiddenCount: Int = defense.cards.size

    def reveal: Open = Open(attack, Defense(defense.cards, Defense.Open))
  }
}
