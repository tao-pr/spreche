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
      // Without modal verb
      case EmptyClaus => renderSVO()
      case ModalVerbClaus(_) => 
        // Modal verb but without verb, 
        // modal verb will become a verb itself
        verb match {
          
          case EmptyClaus => 
            Satze(clauses.map{
              case ModalVerbClaus(v) => VerbClaus(v.toVerb)
              case any => any
            }).render(this)

          case _ => renderSMOV()
        }
    }
  }

  private def renderSMOV()
  (implicit rule: MasterRule) = {
    
    val verbClaus = verb.asInstanceOf[VerbClaus]

    val clausesNoVerb = clauses.filterNot(_.isInstanceOf[VerbClaus])
    val endingVerb = rule.conjugation.conjugateVerb(
      verbClaus.v.v, Wir, NoArtikel
    )
    
    Satze.abbrev(clausesNoVerb.zipWithIndex.map{
      case(c,i) => c.render(this, i).trim
    }.mkString(" ")) + " " + endingVerb
  }

  private def renderSVO()(implicit rule: MasterRule) = {
    val clausesWithoutModalVerb = clauses.filterNot(_.isInstanceOf[ModalVerbClaus])
    val satzeWithoutModalVerb = Satze(clausesWithoutModalVerb)
    Satze.abbrev(clausesWithoutModalVerb.zipWithIndex.map{
      case(c,i) => c.render(satzeWithoutModalVerb, i).trim
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

  private def parseModalVerb(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    implicit val r = rule.conjugation

    val newModalVerb = ModalVerbClaus(ModalVerb.toVerb(s.toLowerCase))
    val newTokens = prevTokens match {
      // [MV]
      case Nil => 
        newModalVerb :: Nil

      // P + [MV]
      case ns :+ (subj@SubjectClaus(_, _)) => subj match {
        // Ihr + [MV]
        case SubjectClaus(Ihre, NP) =>
          ns ++ Seq(SubjectClaus(NoArtikel, Ihr), newModalVerb)

        // _ + P + [MV]
        case _ =>
          prevTokens :+ newModalVerb
      }
    
      // otherwise
      case _ =>
        println(s"Misplaced modal verb : ${s}")
        prevTokens
    }

    parse(others, newTokens)
  }

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
        if (ModalVerb.isInstance(s)){
          parseModalVerb(prevTokens, others, s)
        }
        else if (Verb.isInstance(s)) {
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

