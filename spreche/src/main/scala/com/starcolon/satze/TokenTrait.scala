package com.starcolon.satze

private[satze] trait Token
case object NoToken extends Token

trait Artikel extends Token {
  def renderWith(gender: String, c: Case): String
  def matchWith(s: String): Boolean
  override def toString = this.getClass.getName.dropRight(1).split('.').last
}

trait Pronoun extends Token {
  val s: String
  val dativ: String 
  val akkusativ: String
  val possess: String
}
