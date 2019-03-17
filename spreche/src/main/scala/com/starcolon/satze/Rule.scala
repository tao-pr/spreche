package com.starcolon.satze

import org.json4s._
import org.json4s.jackson.JsonMethods._
import scala.io.Source

case class Verb(v: String)

trait Artikel { 
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

trait PMap extends Map[String, String]

case class Ort(place: String, artikel: Artikel)

trait Rule 

object NullRule extends Rule
case class ConjugationRule(m: Map[Verb, PMap]) extends Rule
case class OrtRule(m: Map[String, Ort]) extends Rule

object Rule {
  private def fromFile(fname: String): String = Source
    .fromInputStream(getClass.getResourceAsStream("/" + fname))
    .getLines().mkString("\n")

  def loadContext: Rule = {
    val j = parse(fromFile("conjugation.json"))
    println(j)

    NullRule
  }
}