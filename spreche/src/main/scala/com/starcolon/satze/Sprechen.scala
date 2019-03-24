package com.starcolon.satze

import java.io._
import com.starcolon.satze._

object Sprechen {
  def conversation(implicit rule: MasterRule): Unit = {
    val inputTokens = readLine("Sagen Sie > ").split(" ").filter(_.trim.size > 0)
    val tokens = Satze.parse(inputTokens)

    conversation
  }
}