package com.starcolon.satze

import java.io._
import com.starcolon.satze._

object Sprechen {
  def conversation(implicit rule: Rule): Unit = {
    val ein = readLine("Sagen Sie > ").split(" ").filter(_.trim.size > 0)


    conversation
  }
}