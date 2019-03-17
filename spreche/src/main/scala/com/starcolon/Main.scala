package com.starcolon

import com.starcolon.satze.{Satze, Rule, Sprechen}

object Main extends App {

  println("Spreche!")

  Sprechen.conversation(Rule.loadContext)
}