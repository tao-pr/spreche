package com.starcolon.satze

sealed trait Token
case object NoToken extends Token

case class Verb(v: String) extends Token {
  def isAkkusativ: Boolean = v != "sein"
  override def toString = v
}

object Verb {
  /**
   * Parse a string to verb (deconjugated)
   */
  def toVerb(s: String)(implicit rule: ConjugationRule): Verb = {
    Verb(rule.deconjugateVerb(s))
  }
}

trait Case 
case object Nominativ extends Case
case object Akkusativ extends Case
case object Dativ extends Case

trait FrageWort extends Token {
  val s: String
  override def toString = s
}
case object Wie extends FrageWort { override val s = "wie" }
case object Wo extends FrageWort { override val s = "wo" }
case object Warum extends FrageWort { override val s = "warum" }
case object Wann extends FrageWort { override val s = "wann" }
case object Wer extends FrageWort { override val s = "wer" }

trait Artikel extends Token {
  def renderWith(gender: String, c: Case): String
}

case object Ein extends Artikel {
  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "ein"
      case "die" => "eine"
      case "das" => "ein"
    }
    case Akkusativ => gender match {
      case "der" => "einen"
      case "die" => "eine"
      case "das" => "ein"
    }
    case Dativ => gender match {
      case "der" => "einem"
      case "die" => "einer"
      case "das" => "einem"
    }
  }
}

case object Der extends Artikel {
  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "der"
      case "die" => "die"
      case "das" => "das"
    }
    case Akkusativ => gender match {
      case "der" => "den"
      case "die" => "die"
      case "das" => "das"
    }
    case Dativ => gender match {
      case "der" => "dem"
      case "die" => "der"
      case "das" => "dem"
    }
  }
}

case object Plural extends Artikel {
  override def renderWith(gender: String, c: Case) = "viele"
}

case object Kein extends Artikel {
  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "kein"
      case "die" => "keine"
      case "das" => "kein"
    }
    case Akkusativ => gender match {
      case "der" => "keinen"
      case "die" => "keine"
      case "das" => "kein"
    }
    case Dativ => gender match {
      case "der" => "keinem"
      case "die" => "keiner"
      case "das" => "keinem"
    }
  }
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

case class Instance(override val s: String) extends Pronoun {
  // TAOTODO: Following remain unused, need to be redesigned
  override val akkusativ = ""
  override val dativ = ""
  override val possess = ""
}

object Pronoun {
  private lazy val infinitivPronouns = List(Ich, Du, Sie, Wir, Ihr, Er, Es)
  private lazy val infinitivPronounStrings = infinitivPronouns.map(_.s)
  def isInfinitiv(s: String) = infinitivPronounStrings.contains(s)
  def isInfinitiv(p: Pronoun) = infinitivPronouns.contains(p)
}

case class Ort(place: String, artikel: Artikel) extends Token
