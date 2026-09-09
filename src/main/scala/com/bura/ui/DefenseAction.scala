package com.bura.ui

sealed trait DefenseAction

object DefenseAction {
  case object Defend extends DefenseAction
  case object Surrender extends DefenseAction
}
