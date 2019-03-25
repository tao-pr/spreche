package com.starcolon.satze

import com.starcolon.satze._
import com.starcolon.satze.Implicits._
import Console._

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

  private def parseVerb(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    implicit val r = rule.conjugation
    val newTokens = prevTokens match {
      // Capture Ihr being previously captured as artikel
      // then Ihr has to become a pronoun
      case _ :+ SubjectClaus(NP, adj, Ihre) => 
        prevTokens.dropRight(1) ++ Seq(SubjectClaus(Ihr, adj, NoArtikel), VerbClaus(Verb.toVerb(s.toLowerCase)))
      // Any
      case _ => 
        prevTokens :+ VerbClaus(Verb.toVerb(s.toLowerCase))
    }
    parse(others, newTokens)
  }

  private def parsePronoun(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    // Akkusativ noun
    val p = Pronoun.toPronoun(s)
    val newTokens = prevTokens match {
      // Pronoun at the beginning of the sazte
      case Nil => prevTokens :+ SubjectClaus(p)
      // Pronoun after an artikel
      case _ :+ ObjectClaus(_,dativP,artikel) => 
        prevTokens.dropRight(1) :+ ObjectClaus(p,dativP,artikel)
      // Pronoun after an artikel
      case _ :+ SubjectClaus(_,adj,artikel) => 
        prevTokens.dropRight(1) :+ SubjectClaus(p,adj,artikel)
      // Otherwise
      case _ => prevTokens :+ ObjectClaus(p)
    }
    parse(others, newTokens)
  }

  private def parseArtikel(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    // Artikel of an akkusativ noun
    val a = Artikel.toArtikel(s)
    // NOTE: "Ihr" will also be counted as artikel initially
    val newTokens = prevTokens match {
      // Artikel of subject
      case Nil  => SubjectClaus(NP,None,a) :: Nil
      // Artikel of object
      case _ => prevTokens :+ ObjectClaus(NP,None,a)
    }
    parse(others, newTokens)
  }

  def parse(tokens: Seq[String], prevTokens: Seq[Claus] = Nil)(implicit rule: MasterRule): Satze = 
    tokens match {
      case s :: others => 
        if (isVerb(s)) {
          parseVerb(prevTokens, others, s)
        }
        else if (isArtikel(s)){
          parseArtikel(prevTokens, others, s)
        }
        else if (isPronoun(s)) {
          parsePronoun(prevTokens, others, s)
        }
        else {
          println(YELLOW_B + "Unknown token : " + RESET + RED + s + RESET)
          parse(others, prevTokens)
        }
      case Nil => Satze(prevTokens)
    }
}

