package com.bura.game

import com.bura.model.{Attack, WonTrick}

sealed trait Round

object Round {

  case object WaitingAttack extends Round
  case class WaitingDefense(attack: Attack) extends Round
  case class DefenderWon(trick: WonTrick.Open) extends Round
  case class AttackerWon(trick: WonTrick.Open) extends Round
  case class AttackerWonHidden(openTrick: WonTrick.Open, hiddenTrick: WonTrick.Hidden) extends Round


}
