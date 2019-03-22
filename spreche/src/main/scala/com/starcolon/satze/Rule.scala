package com.starcolon.satze

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.Formats
import scala.io.Source

import Console.{CYAN,GREEN,YELLOW,RED,MAGENTA,RESET}

sealed trait Rule 

object NullRule extends Rule

case class ConjugationRule(m: Map[String, Map[String, String]]) extends Rule {

  lazy val reverseMap = m.toList.flatMap{ case (v, n) => 
    n.map{ case(_,w) => (w, v) }
  }.distinct.toMap
  
  def deconjugateVerb(v: String): String = {
    reverseMap.getOrElse(v, v)
  }
  
  def conjugateVerb(v: String, p: Pronoun) = m.getOrElse(v, Map(p.s -> v)).getOrElse(p.s, v)

  override def toString = m.map{ case(_, n) => 
    n.map{ case(p, v) => s"${p} ${v}"}.mkString(" | ") 
  }.mkString("\n")
}

sealed case class Sache(s: String, artikel: Artikel, plural: String) extends Rule {
  override def toString = s"${artikel.s} $s"
}

case class SacheRule(m: Map[String, Sache]) extends Rule {
  def findGender(s: String): String = m.getOrElse(s, "")
  override def toString = m.map{ case(_, sache) => sache.toString }.mkString("\n")
}

case class OrtRule(m: Map[String, Ort]) extends Rule

case class MasterRule(
  conjugation: ConjugationRule, 
  sache: SacheRule, 
  ort: OrtRule) 
extends Rule

object Rule {

  private def fromFile(fname: String): String = Source
    .fromInputStream(getClass.getResourceAsStream("/" + fname))
    .getLines().mkString("\n")

  def loadConjugationRule: ConjugationRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    ConjugationRule(parse(fromFile("conjugation.json")).extract[Map[String, Map[String, String]]])
  }

  def loadSacheRule: SacheRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    SacheRule(parse(fromFile("sache.json")).extract[Map[String, List[String]]].map{
      case (sache, ns) => ns.head match {
        case "der" => (sache, Sache(sache, Der, ns.last))
        case "die" => (sache, Sache(sache, Die, ns.last))
        case "das" => (sache, Sache(sache, Das, ns.last))
      }
    })
  }

  def loadContext: Rule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    val conjugation = loadConjugationRule
    val sache = loadSacheRule
    
    println(GREEN)
    println("Rules loaded")
    println(RESET)

    println(conjugation)
    println(sache)
    
    MasterRule(conjugation, sache, OrtRule(Map.empty))
  }
}