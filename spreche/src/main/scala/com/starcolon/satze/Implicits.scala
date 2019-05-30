package com.starcolon.satze

import sys.process._


object Implicits {
  implicit class StrOps(val s: String) extends AnyVal {
    def capInitial = if (s.trim.isEmpty) s.trim else s.take(1).toUpperCase + s.tail.toLowerCase

    def speak = ("say -v Anna " + s) !

    /**
     * [s] as a verb, removing separable prefix from it
     */
    def ohnePrefix(implicit rule: ConjugationRule): String = 
      rule.separate(s).map{_._2}.getOrElse(s)
  }
}
  