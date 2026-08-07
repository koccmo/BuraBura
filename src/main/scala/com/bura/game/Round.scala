package com.bura.game

import com.bura.model.{Attack, Defense, WonTrick}

sealed trait Round

case object Round {

  case object WaitingAttack extends Round
  case class WaitingDefense(attack: Attack) extends Round
  case class Finished(trick: WonTrick) extends Round

}
