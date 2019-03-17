package com.starcolon.satze

import com.starcolon.satze.Rule

trait Satze {
  def toString: String
  def who: Option[String]
  def action: Option[String]
  def where: Option[String]
  def when: Option[String]
  def indirectNoun: Option[String]
  def directNoun: Option[String]
}

object Satze {
  def conversation(implicit rule: Rule): Unit = ???
}