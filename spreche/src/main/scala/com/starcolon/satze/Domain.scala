package com.starcolon.satze

sealed trait Token

case class Verb(v: String) extends Token {
  override def toString = v
}

trait FrageWort extends Token {
  val s: String
  override def toString = s
  // TAOTODO: Dativ and Akkusativ form
}
case object Wie extends FrageWort { override val s = "wie" }
case object Wo extends FrageWort { override val s = "wo" }
case object Warum extends FrageWort { override val s = "warum" }
case object Wann extends FrageWort { override val s = "wann" }
case object Wer extends FrageWort { override val s = "wer" }

trait Artikel extends Token { 
  val s: String
  val a: String
  val akkusativ: String
  val dativ: String
  val zu: String
  val in: String
  def stemAkkusativ(s: String) = s
  def stemDativ(s: String) = s
  // TAOTODO: Add possessive artikel modes
}
case object Der extends Artikel{
  override val s = "der"
  override val a = "ein"
  override val akkusativ = "einen"
  override val dativ = "einem"
  override val zu = "zum"
  override val in = "im"
  override def stemAkkusativ(s: String) = s + "en"
  override def stemDativ(s: String) = s + "em"
}
case object Die extends Artikel{
  override val s = "die"
  override val a = "eine"
  override val akkusativ = "eine"
  override val dativ = "einen"
  override val zu = "zu die"
  override val in = "in die"
  override def stemAkkusativ(s: String) = s + "e"
  override def stemDativ(s: String) = s + "er"
}
case object Das extends Artikel{
  override val s = "das"
  override val a = "ein"
  override val akkusativ = "ein"
  override val dativ = "einem"
  override val zu = "zum"
  override val in = "ins"
  override def stemDativ(s: String) = s + "em"
}
case object Plural extends Artikel{
  override val s = "die"
  override val a = "viele"
  override val akkusativ = "eine"
  override val dativ = "einer"
  override val zu = "zu den"
  override val in = "in die"
  override def stemDativ(s: String) = s + "en"
}

trait Pronoun extends Token {
  val s: String
  val dativ: String 
  val akkusativ: String
  val possess: String
}
case object Ich extends Pronoun{
  override val s = "ich"
  override val akkusativ = "mich"
  override val dativ = "mir"
  override val possess = "mein"
}
case object Du extends Pronoun{
  override val s = "idu"
  override val akkusativ = "dich"
  override val dativ = "dir"
  override val possess = "dein"
}
case object Sie extends Pronoun{
  override val s = "sie"
  override val akkusativ = "sie"
  override val dativ = "sie"
  override val possess = "ihr"
}
case object Er extends Pronoun{
  override val s = "er"
  override val akkusativ = "ihn"
  override val dativ = "ihm"
  override val possess = "seid"
}
case object Es extends Pronoun{
  override val s = "es"
  override val akkusativ = "es"
  override val dativ = "es"
  override val possess = "ihr"
}
case object Wir extends Pronoun{
  override val s = "wir"
  override val akkusativ = "uns"
  override val dativ = "uns"
  override val possess = "unser"
}
case object Ihr extends Pronoun{
  override val s = "ihr"
  override val akkusativ = "euch"
  override val dativ = "euch"
  override val possess = "euer"
}
// case object Da extends Pronoun {
//   override val s = "das"
//   override val akkusativ = "das"
//   override val dativ = "das"
//   override val possess = "ihr"
// }
case class Person(override val s: String, p: Pronoun) extends Pronoun {
  override val akkusativ = p.akkusativ
  override val dativ = p.dativ
  override val possess = p.possess 
}

case class Ort(place: String, artikel: Artikel) extends Token

class Preposition(s: String) extends Token {
  def akkusativ(a: Artikel): String = a match {
    case Der => s + " den"
    case Die => s + " die"
    case Das => s + " das"
    case Plural => s
  }
  def dativ(a: Artikel): String = a match {
    case Der => s + " dem"
    case Die => s + " der"
    case Das => s + " dem"
    case Plural => s + " den"
  }
}