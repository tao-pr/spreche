package com.starcolon.satze

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.Formats
import scala.io.Source

sealed trait Token

case class Verb(v: String) extends Token

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

trait PMap extends Map[String, String]

case class Ort(place: String, artikel: Artikel)

trait Rule 

object NullRule extends Rule
case class ConjugationRule(m: Map[String, Map[String, String]]) extends Rule {
  def conjugateVerb(v: String, p: Pronoun) = m.getOrElse(v, Map(p.s -> v)).getOrElse(p.s, v)
  override def toString = m.map{ case(_, n) => 
    n.map{ case(p, v) => s"${p} ${v}"}.mkString(" | ") 
  }.mkString("\n")
}
case class OrtRule(m: Map[String, Ort]) extends Rule

object Rule {
  private def fromFile(fname: String): String = Source
    .fromInputStream(getClass.getResourceAsStream("/" + fname))
    .getLines().mkString("\n")

  def loadConjugationRule: ConjugationRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    ConjugationRule(parse(fromFile("conjugation.json")).extract[Map[String, Map[String, String]]])
  }

  def loadContext: Rule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    val ruleConjugation = loadConjugationRule
    println(ruleConjugation)
    NullRule
  }
}