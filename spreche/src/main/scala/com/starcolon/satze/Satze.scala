package com.starcolon.satze

import com.starcolon.satze.Rule

trait Satze {
  val tokens: Seq[Token]
  def toString: String
}

