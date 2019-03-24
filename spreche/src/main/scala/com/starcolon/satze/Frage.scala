package com.starcolon.satze

trait FrageWort extends Token {
  val s: String
  override def toString = s
}
case object Wie extends FrageWort { override val s = "wie" }
case object Wo extends FrageWort { override val s = "wo" }
case object Warum extends FrageWort { override val s = "warum" }
case object Wann extends FrageWort { override val s = "wann" }
case object Wer extends FrageWort { override val s = "wer" }