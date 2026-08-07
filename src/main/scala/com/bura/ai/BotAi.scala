package com.bura.ai

import com.bura.model.Card

trait BotAi {
  def shooseCard(hand: List[Card]): List[Card]
}
