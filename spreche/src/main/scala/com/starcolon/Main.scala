package com.starcolon

import com.starcolon.satze.Satze
import com.starcolon.satze.Rule

object Main extends App {

  println("Spreche!")

  Satze.conversation(Rule.loadContext)
}