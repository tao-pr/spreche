package com.starcolon.satze

import com.starcolon.satze._
import com.starcolon.satze.Implicits._
import Console._

case class Satze(clauses: Seq[Claus]) extends Claus {
  def subject: Claus = clauses.find(_.isInstanceOf[SubjectClaus]).getOrElse(EmptyClaus)
  def verb: Claus = clauses.find(_.isInstanceOf[VerbClaus]).getOrElse(EmptyClaus)
  def modalVerb: Claus = clauses.find(_.isInstanceOf[ModalVerbClaus]).getOrElse(EmptyClaus)
  def objekt: Claus = clauses.find(_.isInstanceOf[ObjectClaus]).getOrElse(EmptyClaus)
  
  override def render(satze: Satze = this, index: Int = -1)(implicit rule: MasterRule) = {
    modalVerb match {
      case EmptyClaus => renderSVO()
      case m@ModalVerbClaus(_) => renderMSVO(m)
    }
  }

  private def renderMSVO(mv: ModalVerbClaus)
  (implicit rule: MasterRule) = {
    
    val subjectClaus = subject.asInstanceOf[SubjectClaus]
    val modalVerb = mv.v.render(subjectClaus)
    
    modalVerb + Satze.abbrev(clauses.zipWithIndex.map{
      case(c,i) => c.render(this, i).trim
    }.mkString(" "))
  }

  private def renderSVO()(implicit rule: MasterRule) = {
    Satze.abbrev(clauses.zipWithIndex.map{
      case(c,i) => c.render(this, i).trim
    }.mkString(" "))
  }
  
  override def toString = clauses.map(_.toString).mkString(" ")
}

object Satze {

  def abbrev(satze: String) = {
    AbbrevRule.foldLeft(satze){ case(sat, pair) => 
      val (a,b) = pair
      sat.replace(a, b)
    }
  }

  // def isVerb(token: String)(implicit rule: MasterRule) = {
  //   rule.conjugation.isVerb(token.toLowerCase)
  // }

  // def isPronoun(token: String)(implicit rule: MasterRule) = {
  //   (Pronoun.isInfinitiv(token.toLowerCase)) || rule.sache.isSache(token.capInitial)
  // }

  // def isArtikel(token: String)(implicit rule: MasterRule) = {
  //   val artikels = (Seq("die","das","eine","ein","kein","keine") ++ Seq("d","ein","kein").flatMap{(a) => 
  //     Seq("er","en","em").map(a + _)
  //   }) ++ Seq("viel", "viele")
  //   def expand(p: Pronoun) = Seq("","e","en","em").map(p.possess + _)
  //   val possArtikels = Pronoun.infinitivPronouns.flatMap(expand)
  //   artikels.contains(token.toLowerCase) || possArtikels.contains(token.toLowerCase)
  // }

  // def isAdj(token: String)(implicit rule: MasterRule) = ???

  // def isPreposition(token: String)(implicit rule: MasterRule) = 
  //   PrepositionRule.isPreposition(token)

  private def parseVerb(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    implicit val r = rule.conjugation
    val newTokens = prevTokens match {
      // Ihr + [V]
      case _ :+ SubjectClaus(Ihre, NP) => 
        prevTokens.dropRight(1) ++ Seq(SubjectClaus(NoArtikel, Ihr), VerbClaus(Verb.toVerb(s.toLowerCase)))
      
      // Any + [V]
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
      // [P]
      case Nil => prevTokens :+ SubjectClaus(p=p)
      
      // _ + artikel + [P]
      case _ :+ SubjectClaus(artikel, NP) => 
        prevTokens.dropRight(1) :+ SubjectClaus(artikel, p)

      // _ + prep + artikel + [P]
      case ns :+ ObjectClaus(prep, artikel, NP) => 
        ns :+ ObjectClaus(prep, artikel, p)

      // _ + prep + artikel + P + [P]
      case _ :+ ObjectClaus(_,_,_) =>
        prevTokens :+ ObjectClaus(None, NoArtikel, p)
      
      // _ + [P]
      case _ => prevTokens :+ ObjectClaus(p=p)
    }
    parse(others, newTokens)
  }

  private def parseArtikel(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    val a = Artikel.toArtikel(s)
    // NOTE: "Ihr" will also be counted as artikel initially
    val newTokens = prevTokens match {
      // [artikel]
      case Nil  => SubjectClaus(a, NP) :: Nil
      
      // _ + prep + [artikel]
      case ns :+ ObjectClaus(prep, _, NP) => 
        ns :+ ObjectClaus(prep, a, NP)
      
      // _ + prep + artikel + P + [artikel]
      case _ :+ (obj@ObjectClaus(_,_,_)) => 
        prevTokens :+ ObjectClaus(None, a, NP)

      // _ + [artikel]
      case _ => prevTokens :+ ObjectClaus(None, a, NP)
    }
    parse(others, newTokens)
  }

  private def parsePreposition(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    val prep = Preposition(s.trim.toLowerCase)
    val newTokens = prevTokens match {
      // [prep]
      case Nil => 
        println(RED + s"A new sentence cannot start with : $s" + RESET)
        prevTokens

      // _ + prep + artikel + P + [prep]
      case _ :+ ObjectClaus(_,_,_) =>
        prevTokens :+ ObjectClaus(Some(prep), NoArtikel, NP)
      
      // _ + [prep]
      case _ => 
        prevTokens :+ ObjectClaus(Some(prep), NoArtikel, NP)
    }
    parse(others, newTokens)
  }

  def parse(tokens: Seq[String], prevTokens: Seq[Claus] = Nil)(implicit rule: MasterRule): Satze = 
    tokens match {
      case s :: others => 
        if (Verb.isInstance(s)) {
          parseVerb(prevTokens, others, s)
        }
        else if (Preposition.isInstance(s)){
          parsePreposition(prevTokens, others, s)
        }
        else if (Artikel.isInstance(s)){
          parseArtikel(prevTokens, others, s)
        }
        else if (Pronoun.isInstance(s)) {
          parsePronoun(prevTokens, others, s)
        }
        else {
          println(YELLOW_B + "Unknown token : " + RESET + RED + s + RESET)
          parse(others, prevTokens)
        }
      case Nil => Satze(prevTokens)
    }
}

