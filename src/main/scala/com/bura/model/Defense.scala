package com.bura.model

import com.bura.model.Defense.Open

case class Defense(cards: List[Card], mode: Defense.Mode = Open) {
  require(
    cards.nonEmpty && cards.size <= 3,
    "Defense must contain 1-3 cards"
  )

  def isHidden: Boolean =
    mode == Defense.Hidden

  def size: Int =
    cards.size

  def points: Int = cards.map(_.points).sum
}

object Defense {
  sealed trait Mode

  case object Open extends Mode
  case object Hidden extends Mode
}