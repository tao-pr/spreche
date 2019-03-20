package com.starcolon.satze

import com.starcolon.satze.{Token, Satze, Frage, Rule}
import com.starcolon.satze.{Wie, Wo, Warum, Wann, Wer}

object Sprechen {
  def conversation(implicit rule: Rule): Unit = {
    // Generate a frage
    val f = generateFrage
    f.print
    val a = readInput("Sagen Sie > ")


    conversation
  }
  
  def generateFrage()(implicit rule: Rule): Frage = {
    
  }
  
  def takeAntwort(frage: Frage)(implicit rule: Rule) = ??? 
}