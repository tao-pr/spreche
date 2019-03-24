package com.starcolon.satze

import com.starcolon.satze._
import com.starcolon.satze.Implicits._
import Console._

trait Claus {
  def render(satze: Satze)(implicit rule: MasterRule): String = toString
}

case object EmptyClaus extends Claus

case class SubjectClaus(p: Pronoun, adj: Option[String] = None) extends Claus {
  override def render(satze: Satze)(implicit rule: MasterRule) = p.s.capInitial
  override def toString = s"-${CYAN_B}S${RESET}:${p.s.capInitial}"
}

case class VerbClaus(v: Verb) extends Claus {
  override def render(satze: Satze)(implicit rule: MasterRule) = satze.subject match {
    case SubjectClaus(p, _) => rule.conjugation.conjugateVerb(v.v, p)
  }
  override def toString = s"-${YELLOW_B}V${RESET}:${v.v}"
}

case class ObjectClaus(directNoun: Pronoun, dativNoun: Option[Pronoun] = None, artikel: Artikel = Ein) extends Claus {
  private def renderInfinitiv(c: Case) = c match {
    case Nominativ => directNoun.s
    case Akkusativ => directNoun.akkusativ
    case Dativ => directNoun.dativ
  }

  private def renderSache(c: Case)(implicit rule: MasterRule) = 
    artikel.renderWith(rule.sache.findGender(directNoun.s.capInitial), c) + " " + directNoun.s.capInitial

  override def render(satze: Satze)(implicit rule: MasterRule) = {
      satze.verb match {
        case VerbClaus(v) => Pronoun.isInfinitiv(directNoun) match {
          case true => renderInfinitiv(if (v.isAkkusativ) Akkusativ else Nominativ)
          case false => renderSache(if (v.isAkkusativ) Akkusativ else Nominativ)
        }
    }
  }

  override def toString = s"-${CYAN_B}O${RESET}:${artikel.toString} ${directNoun.s}"
}

case class Satze(clauses: Seq[Claus]) extends Claus {
  def subject: Claus = clauses.find(_.isInstanceOf[SubjectClaus]).getOrElse(EmptyClaus)
  def verb: Claus = clauses.find(_.isInstanceOf[VerbClaus]).getOrElse(EmptyClaus)
  def objekt: Claus = clauses.find(_.isInstanceOf[ObjectClaus]).getOrElse(EmptyClaus)
  override def render(satze: Satze = this)(implicit rule: MasterRule) = {
    clauses.map(_.render(this).trim).mkString(" ")
  }
  override def toString = clauses.map(_.toString).mkString(" ")
}

object Satze {
  // TAOTODO: All is__ functions can be made generic
  def isVerb(token: String)(implicit rule: MasterRule) = {
    rule.conjugation.isVerb(token.toLowerCase)
  }

  def isPronoun(token: String)(implicit rule: MasterRule) = {
    (Pronoun.isInfinitiv(token.toLowerCase)) || rule.sache.isSache(token.capInitial)
  }

  def isArtikel(token: String)(implicit rule: MasterRule) = {
    val artikels = (Seq("die","das","eine","ein") ++ Seq("d","ein").flatMap{(a) => 
      Seq("er","en","em").map(a + _)
    })
    def expand(p: Pronoun) = Seq("","e","en","em").map(p.possess + _)
    val possArtikels = Pronoun.infinitivPronouns.flatMap(expand)
    artikels.contains(token.toLowerCase) || possArtikels.contains(token.toLowerCase)
  }

  def isAdj(token: String)(implicit rule: MasterRule) = ???

  def isPreposition(token: String)(implicit rule: MasterRule) = ???

  def parse(tokens: Seq[String], prevTokens: Seq[Claus] = Nil)(implicit rule: MasterRule): Satze = 
    tokens match {
      case s :: others => 
        // TAOTODO: Modularise following cases
        if (isVerb(s)) {
          implicit val r = rule.conjugation
          val newTokens = prevTokens ++ Seq(VerbClaus(Verb.toVerb(s.toLowerCase)))
          parse(others, newTokens)
        }
        else if (isArtikel(s)){
          // Artikel of an akkusativ noun
          val a = Artikel.toArtikel(s)
          val newTokens = prevTokens ++ Seq(ObjectClaus(Ich,None,a))
          parse(others, newTokens)
        }
        else if (isPronoun(s)) {
          // Akkusativ noun
          val p = Pronoun.toPronoun(s)
          val newTokens = prevTokens match {
            case Nil => prevTokens ++ Seq(SubjectClaus(p))
            case _ :+ ObjectClaus(_,dativP,artikel) => prevTokens.dropRight(1) ++ Seq(ObjectClaus(p,dativP,artikel))
            case _ => prevTokens ++ Seq(ObjectClaus(p))
          }
          parse(others, newTokens)
        }
        else {
          println(YELLOW_B + "Unknown token : " + RESET + RED + s + RESET)
          parse(others, prevTokens)
        }
      case Nil => Satze(prevTokens)
    }
}

