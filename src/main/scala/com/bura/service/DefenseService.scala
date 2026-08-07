package com.bura.service

import com.bura.model.{Attack, Card, Defense, Suit}
import com.bura.rules.Rules

object DefenseService {

  def possibleDefense(
    attack: Attack,
    hand: List[Card],
    trump: Suit
  ): List[Defense] =
    hand
      .combinations(attack.cards.size)
      .flatMap(cards => createDefense(cards, attack, trump))
      .toList

  private def createDefense(
    cards: List[Card],
    attack: Attack,
    trump: Suit
  ): Option[Defense] = {
    val defense = Defense(cards)

    if (Rules.beatsAttack(attack, defense, trump))
      Some(defense)
    else
      None
  }

  def bestDefense(
    attack: Attack,
    hand: List[Card],
    trump: Suit
  ): Option[Defense] =
    possibleDefense(attack, hand, trump).sortBy(totalStrength).headOption

  private def totalStrength(defense: Defense): Int =
    defense.cards.map(_.rank.strength).sum
}
