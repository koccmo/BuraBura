package com.bura

import com.bura.model.{Attack, Card, Defense}
import com.bura.model.Rank.{Ace, Eight, Jack, King, Nine, Queen, Seven, Six, Ten}
import com.bura.model.Suit.{Clubs, Hearts, Spades}
import com.bura.rules.Rules

object TryCode extends App {

  val rulesCheck = Rules

  val cardAttackFirst = List(Card(Seven, Hearts), Card(Ten, Hearts),Card(Nine, Hearts))
  val cardsAttackSecond = List(Card(Six, Spades), Card(Ace, Spades))

  val cardsDefenseFirst = List(Card(Jack, Hearts), Card(Ace, Hearts), Card(Eight, Hearts))
  val cardsDefenseSecond = List(Card(Nine, Spades), Card(Ten, Spades))

  println(rulesCheck.beatsAttack(Attack(cardAttackFirst), Defense(cardsDefenseFirst), Hearts))
  println(rulesCheck.beatsAttack(Attack(cardsDefenseFirst), Defense(cardAttackFirst), Hearts))
  println(cardAttackFirst.patch(0, cardsAttackSecond, 1))
  println(rulesCheck.beatsAttack(Attack(List(Card(Seven, Hearts), Card(Eight, Hearts))), Defense(List(Card(Jack, Hearts), Card(Queen, Hearts))), Hearts))
  println(rulesCheck.beatsAttack(Attack(List(Card(Jack, Spades), Card(King, Spades))), Defense(List(Card(Six, Hearts), Card(Queen, Spades))), Hearts))
  println("-------")
  println(cardAttackFirst.permutations.toList)
  println("________")
  val numbers = List(1,2,3)
  println((1 to 3).iterator.flatMap(numbers.combinations))

  val ok: Option[Int] = Some(3)
  println(ok)
  println(ok.isDefined)
}
