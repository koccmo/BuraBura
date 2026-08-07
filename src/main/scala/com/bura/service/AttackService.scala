package com.bura.service

import com.bura.model.{Attack, Card}

import scala.util.Try

object AttackService {

  def possibleAttacks(hand: List[Card]): List[Attack] =
    (1 to math.min(3, hand.size))
      .iterator
      .flatMap(hand.combinations)
      .flatMap(createAttack)
      .toList

  private def createAttack(cards: List[Card]): Option[Attack] =
    Try(Attack(cards)).toOption

}
