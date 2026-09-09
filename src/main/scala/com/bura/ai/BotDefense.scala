package com.bura.ai

import com.bura.game.GameState
import com.bura.model.{Attack, Card, Defense, Player, Suit}
import com.bura.model.Player.Robot
import com.bura.rules.Rules

import scala.util.Try

object BotDefense {

  def choose(
    state: GameState,
    robot: Player.Robot,
    attack: Attack
  ): Option[Defense] = {

    val analysis         = HandAnalysis.from(robot.hand, state.trump)
    val beatableDefenses = possibleDefenses(robot.hand, attack).filter { defense =>
      Rules.beatsAttack(attack, defense, state.trump)
    }

    if (beatableDefenses.isEmpty) {
      None
    } else {
      chooseIfCanBeat(state, attack, analysis, beatableDefenses)
    }
  }

  def chooseSurrenderDefense(
    state: GameState,
    robot: Player.Robot,
    attack: Attack
  ): Option[Defense] = {

    val analysis = HandAnalysis.from(robot.hand, state.trump)

    val cards: List[Card] =
      attack.cards.size match {
        case 1 => surrenderOneCard(analysis)
        case 2 => surrenderTwoCards(analysis)
        case 3 => surrenderThreeCards(analysis)
        case _ => Nil
      }

    if (cards.size == attack.cards.size) Some(Defense(cards))
    else { None }
  }

  private def chooseIfCanBeat(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis,
    beatableDefenses: List[Defense]
  ): Option[Defense] = {

    val attackUsesTrump = attack.cards.exists(_.suit == state.trump)
    val attackAmount    = attack.cards.size
    val trumpAmount     = analysis.trumpCards.size

    val strategyDefense: Option[Defense] =
      if (attackUsesTrump && attackAmount == 1 && trumpAmount == 1) {
        bothOneTrump(state, attack, analysis)
      } else if (attackUsesTrump && attackAmount == 2 && trumpAmount == 2) {
        bothTwoTrumps(state, attack, analysis)
      } else if (!attackUsesTrump && analysis.trumpCards.isEmpty) {
        bothNoTrumps(state, attack, analysis)
      } else { defenderHasTrumps(state, attack, analysis) }

    strategyDefense.orElse(chooseBestDefense(beatableDefenses))
  }

  private def possibleDefenses(hand: List[Card], attack: Attack): List[Defense] =
    hand
      .combinations(attack.cards.size)
      .flatMap { cards => Try(Defense(cards)).toOption }
      .toList

  private def createDefense(
    cards: List[Card],
    attack: Attack,
    trump: Suit
  ): Option[Defense] =
    cards
      .combinations(attack.cards.size)
      .flatMap { combination => Try(Defense(combination)).toOption }
      .find { defense => Rules.beatsAttack(attack, defense, trump) }

  private def chooseBestDefense(defenses: List[Defense]): Option[Defense] =
    defenses.minByOption(defenseCost)

  private def defenseCost(defense: Defense): (Int, Int) =
    (defense.points, defense.cards.map(_.rank.strength).sum)

  private def cheapestCards(cards: List[Card], amount: Int): List[Card] =
    cards
      .sortBy { card => (card.points, card.rank.strength) }
      .take(amount)

  private def weakestCard(cards: List[Card]): Option[Card] =
    cards.minByOption { card => (card.points, card.rank.strength) }

  private def bothOneTrump(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = {

    val attackPoints   = attack.points
    val trumpPoints    = analysis.trumpPoints
    val nonTrumpPoints = analysis.nonTrumpPoints
    val suitsAmount    = analysis.suitsAmount
    val attackAmount   = attack.cards.size

    if (attackPoints == 0 && trumpPoints == 0 && nonTrumpPoints == 21) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints == 0 && suitsAmount == 2) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints == 0 && nonTrumpPoints <= 15 && suitsAmount == 3) {
      createDefense(analysis.nonTrumpCards.drop(attackAmount), attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints >= 2 && trumpPoints <= 4 && suitsAmount == 2) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints >= 2 && trumpPoints <= 4 && suitsAmount == 3) {
      createDefense(analysis.nonTrumpCards.drop(attackAmount), attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints <= 10 && nonTrumpPoints == 21) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints <= 10 && nonTrumpPoints <= 15 && suitsAmount == 2) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints == 0 && trumpPoints <= 10 && nonTrumpPoints <= 15 && suitsAmount == 3) {
      createDefense(analysis.nonTrumpCards.drop(attackAmount), attack, state.trump)
    } else if (attackPoints >= 2 && nonTrumpPoints == 21) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints >= 2 && suitsAmount == 2) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints >= 2 && nonTrumpPoints <= 15 && suitsAmount == 2) {
      createDefense(analysis.trumpCards, attack, state.trump)
    } else if (attackPoints >= 2 && nonTrumpPoints <= 15 && suitsAmount == 3) {
      createDefense(analysis.nonTrumpCards.drop(attackAmount), attack, state.trump)
    } else { None }
  }

  private def bothTwoTrumps(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = createDefense(analysis.trumpCards, attack, state.trump)

  private def bothNoTrumps(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = {

    val attackAmount = attack.cards.size

    val attackSuit = attack.cards.head.suit

    val cards =
      if (attackAmount == 1 && analysis.suitsAmount == 3) {
        analysis.nonTrumpCards
          .filter(_.suit == attackSuit)
      } else if (attackAmount == 1 && analysis.suitsAmount == 2) {
        analysis.nonTrumpCards
          .filter(_.suit == attackSuit)
          .drop(attackAmount)
      } else if (attackAmount == 2) {
        analysis.nonTrumpCards
          .filter(_.suit == attackSuit)
      } else {
        analysis.nonTrumpCards
      }

    createDefense(cards, attack, state.trump)
  }

  private def defenderHasTrumps(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = {

    val attackAmount = attack.cards.size
    val trumpAmount  = analysis.trumpCards.size

    if (attackAmount == 1 && trumpAmount == 1) {
      oneAttackOneTrump(state, attack, analysis)
    } else if (attackAmount == 1 && trumpAmount == 2) {
      oneAttackTwoTrumps(state, attack, analysis)
    } else if (attackAmount == 2 && trumpAmount == 1) {
      twoAttackOneTrump(state, attack, analysis)
    } else if (attackAmount == 2 && trumpAmount == 2) {
      twoAttackTwoTrumps(state, attack, analysis)
    } else { createDefense(analysis.cards, attack, state.trump) }
  }

  private def oneAttackOneTrump(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = {

    val attackCard          = attack.cards.head
    val sameSuitCards       = analysis.nonTrumpCards
      .filter(_.suit == attackCard.suit)
    val canBeatWithoutTrump =
      sameSuitCards.exists { card => Rules.beatsOne(attackCard, card, state.trump) }

    val preferredCards =
      if (attack.points == 11 && analysis.suitsAmount == 3 && analysis.nonTrumpPoints >= 20) {
        analysis.trumpCards
      } else if (attack.points == 10 && analysis.suitsAmount == 3 && canBeatWithoutTrump) {
        sameSuitCards
      } else if (attack.points <= 4 && analysis.suitsAmount == 3 && canBeatWithoutTrump) {
        sameSuitCards
      } else { analysis.trumpCards }

    createDefense(preferredCards, attack, state.trump)
  }

  private def oneAttackTwoTrumps(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] =
    createDefense(analysis.nonTrumpCards, attack, state.trump)
      .orElse { createDefense(analysis.trumpCards, attack, state.trump) }

  private def twoAttackOneTrump(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = {

    val attackSuit    = attack.cards.head.suit
    val sameSuitCards = analysis.nonTrumpCards
      .filter(_.suit == attackSuit)

    createDefense(sameSuitCards, attack, state.trump)
      .orElse { createDefense(analysis.nonTrumpCards ++ analysis.trumpCards, attack, state.trump) }
  }

  private def twoAttackTwoTrumps(
    state: GameState,
    attack: Attack,
    analysis: HandAnalysis
  ): Option[Defense] = {

    val oneTrumpDefense = weakestCard(analysis.trumpCards).flatMap { trump =>
      createDefense(analysis.nonTrumpCards :+ trump, attack, state.trump)
    }

    oneTrumpDefense.orElse { createDefense(analysis.trumpCards, attack, state.trump) }
  }

  private def surrenderOneCard(analysis: HandAnalysis): List[Card] = {

    val hasTrump = analysis.trumpCards.nonEmpty

    if (!hasTrump && analysis.suitsAmount == 3) {
      weakestCard(analysis.cards).toList
    } else if (!hasTrump && analysis.suitsAmount == 2) {
      analysis.cards
        .groupBy(_.suit)
        .values
        .find(_.size == 1)
        .flatMap(_.headOption)
        .toList
    } else if (hasTrump && analysis.suitsAmount == 3 && analysis.nonTrumpPoints <= 14) {
      cheapestCards(analysis.nonTrumpCards, 1)
    } else if (hasTrump && analysis.suitsAmount == 2 && analysis.trumpPoints >= 10) {
      cheapestCards(analysis.nonTrumpCards, 1)
    } else { cheapestCards(analysis.cards, 1) }
  }

  private def surrenderTwoCards(analysis: HandAnalysis): List[Card] = {

    val hasTrump = analysis.trumpCards.nonEmpty

    if (!hasTrump) { cheapestCards(analysis.cards, 2)
    } else if (analysis.trumpCards.size == 2 && analysis.nonTrumpCards.nonEmpty) {

      val weakestTrump = weakestCard(analysis.trumpCards)
      val weakestNonTrump = weakestCard(analysis.nonTrumpCards)

      List(weakestNonTrump, weakestTrump).flatten
    } else {
      val preferred = cheapestCards(analysis.nonTrumpCards, 2)

      if (preferred.size == 2) { preferred
      } else { cheapestCards(analysis.cards, 2) }
    }
  }

  private def surrenderThreeCards(analysis: HandAnalysis): List[Card] =
    cheapestCards(analysis.cards, 3)

}
