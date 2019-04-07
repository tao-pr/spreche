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
    val newTokens = prevTokens :+ newModalVerb
    parse(others, newTokens)
  }

  private def parseVerb(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    implicit val r = rule.conjugation
    val newTokens = prevTokens :+ VerbClaus(Verb.toVerb(s.toLowerCase))
    parse(others, newTokens)
  }

  private def parsePronoun(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    // Akkusativ noun
    val p = Pronoun.toPronoun(s)
    val newTokens = prevTokens match {
      // [P]
      case Nil => SubjectClaus((Ein,p) :: Nil) :: Nil

      // Continuous subject
      case ns :+ SubjectClaus(ps,c) => ps match {
        
        // _ + artikel + [P]
        case ps0 :+ ((a0,NP)) => 
          ns :+ SubjectClaus(ps0 :+ ((a0,p)), c)

        // _ + artikel + P + [P] -- missing connector
        case _ => 
          ns :+ SubjectClaus(ps :+ ((Ein,p)), c)
      }

      // Continuous object
      case ns :+ ObjectClaus(prep,ps,c) => ps match {

        // prep + [P]
        case Nil => 
          ns :+ ObjectClaus(prep, ((Ein,p)) :: Nil, c)

        // _ + artikel + [P]
        case ps0 :+ ((a0,NP)) => 
          println(s"adding new pronoun ${p.s} after ${a0.toString}") // TAODEBUG:
          ns :+ ObjectClaus(prep, ps0 :+ ((a0,p)), c)

        // 2nd object
        // _ + artikel + P + [P]
        case _ =>
          println(s"adding new pronoun ${p.s}") // TAODEBUG:
          prevTokens :+ ObjectClaus(prep, ps :+ (Ein,p), c)
      }
      
      // _ + [P]
      case _ => prevTokens :+ ObjectClaus(ps = (Ein,p) :: Nil)
    }
    parse(others, newTokens)
  }

  private def parseArtikel(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {

    val a = Artikel.toArtikel(s)
    val newPs = ((a,NP)) :: Nil

    // NOTE: "Ihr" will also be counted as artikel initially
    val newTokens = prevTokens match {
      // [artikel]
      case Nil  => 
        SubjectClaus(newPs) :: Nil

      // Multiple subjects
      // P + [P]
      case ns :+ SubjectClaus(ps,c) =>
        ns :+ SubjectClaus(ps :+ ((a,NP)), c)

      // _ + prep + [artikel]
      case ns :+ ObjectClaus(prep, ps, c) => ps match {

        // _ + prep + [artikel]
        case Nil => 
          ns :+ ObjectClaus(prep, newPs, c)

        // 2nd object
        // _ + prep + artikel + P + [artikel]
        case _ =>
          prevTokens :+ ObjectClaus(None, newPs)
      }

      // _ + [artikel]
      case _ => 
        prevTokens :+ ObjectClaus(None, newPs)
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

      // 2nd object
      // _ + prep + artikel + P + [prep]
      case _ :+ ObjectClaus(_,_,_) =>
        prevTokens :+ ObjectClaus(Some(prep))
      
      // _ + [prep]
      case _ => 
        prevTokens :+ ObjectClaus(Some(prep))
    }
    parse(others, newTokens)
  }

  private def parseConnector(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    val c = Connector.toConnector(s)
    val newTokens = prevTokens match {

      // _ + P + [con]
      case ns :+ SubjectClaus(ps,_) =>
        ns :+ SubjectClaus(ps, c)

      // _ + P + [con]
      case ns :+ ObjectClaus(prep,ps,_) =>
        ns :+ ObjectClaus(prep, ps, c)

      case _ =>
        println(s"$s is misplaced, will be ignored")
        prevTokens
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
        else if (Connector.isInstance(s)) {
          parseConnector(prevTokens, others, s)
        }
        else {
          println(YELLOW_B + "Unknown token : " + RESET + RED + s + RESET)
          parse(others, prevTokens)
        }
      case Nil => Satze(prevTokens)
    }
}

