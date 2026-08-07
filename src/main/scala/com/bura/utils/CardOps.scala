package com.bura.utils

import com.bura.model.Card

object CardOps {
  def totalPoint(cards: List[Card]): Int = cards.map(_.rank.points).sum
}
