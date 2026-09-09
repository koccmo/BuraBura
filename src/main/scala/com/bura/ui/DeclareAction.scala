package com.bura.ui

sealed trait DeclareAction

object DeclareAction {
  case object Continue extends DeclareAction
  case object Declare31 extends DeclareAction
}
