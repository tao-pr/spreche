package com.starcolon.satze

import com.starcolon.satze.{Token, Satze, Frage, Rule}

object Sprechen {
  def conversation(implicit rule: Rule): Unit = ???
  def generateFrage(): Frage = ??? 
  def takeAntwort(frage: Frage) = ??? 
}