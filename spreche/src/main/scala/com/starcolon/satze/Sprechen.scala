package com.starcolon.satze

import com.starcolon.satze._
import Console._
import com.starcolon.satze.Implicits._

object Sprechen {
  def conversation(implicit rule: MasterRule): Unit = {
    val inputTokens = readLine("Sagen Sie > ").split(" ").filter(_.trim.size > 0).toList
    val inputSatze = Satze.parse(inputTokens)
    println(inputSatze)
    val out = inputSatze.render()
    println(s"${MAGENTA}${out}${RESET}")
    out.speak
    conversation
  }
}