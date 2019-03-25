package com.starcolon.satze

object Implicits {
  implicit class StrOps(val s: String) extends AnyVal {
    def capInitial = if (s.trim.isEmpty) s.trim else s.take(1).toUpperCase + s.tail.toLowerCase
  }
}
  