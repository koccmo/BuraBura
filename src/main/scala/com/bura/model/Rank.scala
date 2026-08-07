package com.bura.model

sealed trait Rank {
  def strength: Int
  def points: Int
  def shortName: String
}

object Rank {

  case object Six extends Rank {
    val points: Int   = 0
    val strength: Int = 1
    val shortName     = "6"
  }

  case object Seven extends Rank {
    val points: Int       = 0
    val strength: Int     = 2
    val shortName: String = "7"
  }

  case object Eight extends Rank {
    val points: Int       = 0
    val strength: Int     = 3
    val shortName: String = "8"
  }

  case object Nine extends Rank {
    val points: Int       = 0
    val strength: Int     = 4
    val shortName: String = "9"
  }

  case object Ten extends Rank {
    val points: Int       = 10
    val strength: Int     = 8
    val shortName: String = "10"
  }

  case object Jack extends Rank {
    val points: Int       = 2
    val strength: Int     = 5
    val shortName: String = "J"
  }

  case object Queen extends Rank {
    val points: Int       = 3
    val strength: Int     = 6
    val shortName: String = "Q"
  }

  case object King extends Rank {
    val points: Int       = 4
    val strength: Int     = 7
    val shortName: String = "K"
  }

  case object Ace extends Rank {
    val points: Int       = 11
    val strength: Int     = 9
    val shortName: String = "A"
  }

  val ValuesList: List[Rank]      = List(Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  val ValuesMap: Map[String, Int] = ValuesList.map(x => x.toString -> x.strength).toMap

}
