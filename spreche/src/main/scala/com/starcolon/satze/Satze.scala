package com.starcolon.satze

import scala.reflect._

import com.starcolon.satze._
import com.starcolon.satze.Implicits._
import Console._

case class Satze(clauses: Seq[Claus]) extends Claus {
  def subject: Option[SubjectClaus]     = clauses.find(_.isInstanceOf[SubjectClaus]).map(_.asInstanceOf[SubjectClaus])
  def verb: Option[VerbClaus]           = clauses.find(_.isInstanceOf[VerbClaus]).map(_.asInstanceOf[VerbClaus])
  def modalVerb: Option[ModalVerbClaus] = clauses.find(_.isInstanceOf[ModalVerbClaus]).map(_.asInstanceOf[ModalVerbClaus])
  def objekt: Option[ObjectClaus]       = clauses.find(_.isInstanceOf[ObjectClaus]).map(_.asInstanceOf[ObjectClaus])
  def time: Option[TimeClaus]           = clauses.find(_.isInstanceOf[TimeClaus]).map(_.asInstanceOf[TimeClaus])
  def negation: Option[NegateClaus]     = clauses.find(_.isInstanceOf[NegateClaus]).map(_.asInstanceOf[NegateClaus])
  def haben: Option[HabenVerbClaus]     = clauses.find(_.isInstanceOf[HabenVerbClaus]).map(_.asInstanceOf[HabenVerbClaus])
  def adv: Option[AdvClaus]             = clauses.find(_.isInstanceOf[AdvClaus]).map(_.asInstanceOf[AdvClaus])
  def allObjekts: Seq[Option[ObjectClaus]] = clauses.collect{ case obj@ObjectClaus(_,_,_) => Some(obj) }.toSeq

  def isPerfekt = clauses.find(_ == HabenVerbClaus).isDefined

  /**
   * Check whether [A] comes before [B] in the sentence
   */
  def isAfter[A<: Claus: ClassTag, B<: Claus: ClassTag] = {
    lazy val clausesIndexed = clauses.zipWithIndex
    val a = clausesIndexed.collect{ case (c,i) if classTag[A].runtimeClass.isInstance(c) => (c,i) }.headOption
    val b = clausesIndexed.collect{ case (c,i) if classTag[B].runtimeClass.isInstance(c) => (c,i) }.headOption

    (a,b) match {
      case (None,None) => false
      case (_, None) => false
      case (None, _) => false
      case (Some((_,ai)), Some((_,bi))) => ai < bi
    }
  }

  def rearranged(implicit rule: MasterRule) = {
    
    lazy val ending = if (verb.map(_.isSeparable).getOrElse(false)) 
      Seq(verb.map(_.prefix)) 
    else Nil
    
    (time, modalVerb, haben) match {

      // With time
      
      case (Some(_), Some(_), _) | (Some(_), _, Some(_)) =>
        Satze({Seq(time, adv, modalVerb, haben, subject) ++ allObjekts ++ Seq(negation, verb)}.flatten)

      case (Some(_), _, _) =>
        Satze({Seq(time, adv, verb, subject) ++ allObjekts ++ Seq(negation) ++ ending}.flatten)

      // Without time

      case (None, Some(_), None) =>
        Satze({Seq(subject, modalVerb, adv) ++ allObjekts ++ Seq(negation, verb)}.flatten)

      case (None, Some(_), Some(_)) =>
        Satze({Seq(subject, haben, adv, verb) ++ allObjekts ++ Seq(negation, modalVerb) ++ ending}.flatten)

      case (None, None, Some(_)) =>
        Satze({Seq(subject, haben, adv) ++ allObjekts ++ Seq(negation, verb)}.flatten)

      case (None, None, None) =>
        Satze({Seq(subject, verb, adv) ++ allObjekts ++ Seq(negation) ++ ending}.flatten)
    }
  }

  override def render(satze: Satze = this, index: Int = -1)(implicit rule: MasterRule) = {
    
    val rearrangedSatze = rearranged
    Satze.abbrev(rearrangedSatze.clauses.zipWithIndex.map{
      case(c,i) => c.render(rearrangedSatze, i).trim
    }.mkString(" ")).trim.replaceAll("  +"," ")
  }

  override def toString = clauses.map(_.toString).mkString(" ")
}

object Satze {

  val excludedTokens = Set("uhr")

  def abbrev(satze: String) = {
    AbbrevRule.foldLeft(satze){ case(sat, pair) => 
      val (a,b) = pair
      sat.replace(a, b)
    }
  }

  def from(clauses: Seq[Claus]) = {

    def correctIhrDas(ps: Seq[(Artikel,Adj,Pronoun)]) = ps.map{ 
      case (Ihre,j,NP) => (NoArtikel,j,Ihr)
      case (Der,j,NP) => (NoArtikel,j,Das)
      case any => any
    }

    // Correction of confusing "Ihr" and "Das" (if any)
    Satze(clauses.map{ 
      case SubjectClaus(ps,c) => SubjectClaus(correctIhrDas(ps),c)
      case ObjectClaus(prep,ps,c) => ObjectClaus(prep,correctIhrDas(ps),c)
      case any => any
    })
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
    // If haben already existed, then convert to a [[HabenVerbClaus]]
    prevTokens.find(_.isInstanceOf[VerbClaus]) match {
      case Some(prevHaben) 
      if Set("seid","haben").contains(
        r.deconjugateVerb(prevHaben.asInstanceOf[VerbClaus].v.v)) =>
        val prevTokensOhneHaben = prevTokens.filterNot(_.isInstanceOf[VerbClaus])
        val newTokens = prevTokensOhneHaben :+ HabenVerbClaus :+ VerbClaus(Verb.toVerb(s.toLowerCase))
        parse(others, newTokens)

      case None =>
        val newTokens = prevTokens :+ VerbClaus(Verb.toVerb(s.toLowerCase))
        parse(others, newTokens)
    }
  }

  private def parsePronoun(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    // Akkusativ noun
    val p = Pronoun.toPronoun(s.toLowerCase)
    val newTokens = prevTokens match {
      // [P]
      case Nil => SubjectClaus((Ein,Adj(Nil),p) :: Nil) :: Nil

      // Continuous subject
      case ns :+ SubjectClaus(ps,c) => ps match {
        
        // _ + artikel + [P]
        case ps0 :+ ((a0,j0,NP)) => 
          ns :+ SubjectClaus(ps0 :+ ((a0,j0,p)), c)

        // _ + artikel + P + [P] -- missing connector
        case _ => 
          ns :+ SubjectClaus(ps :+ ((Ein,Adj(Nil),p)), c)
      }

      // Continuous object
      case ns :+ ObjectClaus(prep,ps,c) => ps match {

        // prep + [P]
        case Nil => 
          ns :+ ObjectClaus(prep, ((Ein,Adj(Nil),p)) :: Nil, c)

        // _ + artikel + [P]
        case ps0 :+ ((a0,j0,NP)) => 
          ns :+ ObjectClaus(prep, ps0 :+ ((a0,j0,p)), c)

        // _ + P + [P]
        case _ => c match {
          // 2nd object
          // _ + artikel + P + [P]
          case Space =>
            prevTokens :+ ObjectClaus(None, ((Ein,Adj(Nil),p)) :: Nil, c)

          // Multiple objects
          // _ + P + und + [P]
          case _ =>
            ns :+ ObjectClaus(prep, ps :+ ((Ein,Adj(Nil),p)), c)
        }
      }

      // _ + [P] as a subject
      case _ if !prevTokens.find(_.isInstanceOf[SubjectClaus]).isDefined => 
        prevTokens :+ SubjectClaus((Ein,Adj(Nil),p) :: Nil)
      
      // _ + [P]
      case _ => 
        prevTokens :+ ObjectClaus(ps = (Ein,Adj(Nil),p) :: Nil)
    }
    parse(others, newTokens)
  }

  private def parseArtikel(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {

    val a = Artikel.toArtikel(s)
    val newPs = ((a,Adj(Nil),NP)) :: Nil

    // NOTE: "Ihr" will also be counted as artikel initially
    val newTokens = prevTokens match {
      // [artikel]
      case Nil  => 
        SubjectClaus(newPs) :: Nil

      // Multiple subjects
      // P + [P]
      case ns :+ SubjectClaus(ps, c) =>
        ns :+ SubjectClaus(ps ++ newPs, c)

      // _ + prep + [artikel]
      case ns :+ ObjectClaus(prep, ps, c) => ps match {

        // _ + prep + [artikel]
        case Nil => 
          ns :+ ObjectClaus(prep, newPs, c)

        
        case _ => c match {
          // 2nd object
          // _ + prep + artikel + P + [artikel]
          case Space => 
            prevTokens :+ ObjectClaus(None, newPs)

          // multiple objects
          // _ P + und + [artikel]
          case _ =>
            ns :+ ObjectClaus(prep, ps ++ newPs, c)
        }
      }

      // _ + [artikel] (as first subject)
      case _ if !prevTokens.find(_.isInstanceOf[SubjectClaus]).isDefined => 
        prevTokens :+ SubjectClaus(newPs)

      // _ + [artikel]
      case _ => 
        prevTokens :+ ObjectClaus(None, newPs)
    }
    parse(others, newTokens)
  }

  private def parseAdv(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    parse(others, prevTokens :+ AdvClaus(s))
  }

  private def parseAdj(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {

    val adj = Adj.deconjugate(s)
    val newPs = ((Ein,Adj(adj :: Nil),NP)) :: Nil

    // NOTE: "Ihr" will also be counted as artikel initially
    val newTokens = prevTokens match {
      // [artikel]
      case Nil  => 
        SubjectClaus(newPs) :: Nil

      // Multiple subjects
      // P + [adj]
      case ns :+ SubjectClaus(ps, c) =>
        val ps_ = ps match {
          case Nil => newPs

          // P + artikel + [adj]
          case ps0 :+ ((a0,Adj(Nil),NP)) => ps0 :+ ((a0,Adj(adj :: Nil),NP))

          // P + artikel + adj + [adj]
          case ps0 :+ ((a0,Adj(adjs0),NP)) => ps0 :+ ((a0,Adj(adjs0 :+ adj),NP))
        }

        ns :+ SubjectClaus(ps_, c)

      // _ + prep + [adj]
      case ns :+ ObjectClaus(prep, ps, c) => ps match {

        // _ + prep + [adj]
        case Nil => 
          ns :+ ObjectClaus(prep, newPs, c)

        // multiple objects
        // _ + prep + ?? + [adj]
        case ps0 :+ ((a0,Adj(j0),p0)) => 

          p0 match {
            // prep + [adj]
            case NP => ns :+ ObjectClaus(prep, ps0 :+ ((a0,Adj(j0 :+ adj),NP)), c)

            // new object begins with adj
            // prep + P + [adj]
            case _ => ns :+ ObjectClaus(prep, ps :+ ((Ein,Adj(adj :: Nil),NP)), c)

          }
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

  private def parseTime(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {

    val newTokens = prevTokens match {

      // _ + T + [__]
      case ns :+ TimeClaus(am,um) => 
        if (s.toLowerCase.trim == "um")
          ns :+ TimeClaus(am, Some(Um("")))
        else if (s.toLowerCase.trim == "am")
          ns :+ TimeClaus(Some(Am("")), um)
        else if (am == Some(Am("")))
          ns :+ TimeClaus(Some(Am(s)), um)
        else if (um == Some(Um("")))
          ns :+ TimeClaus(am, Some(Um(s)))
        else 
          prevTokens

      // _ + [__]
      case _ => s.trim.toLowerCase match {
        case "am" => prevTokens :+ TimeClaus(Some(Am("")), None)
        case "um" => prevTokens :+ TimeClaus(None, Some(Um("")))
        case n if (Time.days.contains(n)) => 
          prevTokens :+ TimeClaus(Some(Am(n)), None)
        case n if (Time.times.contains(n)) => 
          prevTokens :+ TimeClaus(None, Some(Um(n)))
      }
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

  private def parseNegation(prevTokens: Seq[Claus], others: Seq[String], s: String)
  (implicit rule: MasterRule) = {
    // Negation will always remain at the beginning of the sentence
    val newTokens = prevTokens match {
      case Nil               => NegateClaus :: Nil
      case NegateClaus :: _  => prevTokens
      case _                 => NegateClaus +: prevTokens
    }

    parse(others, newTokens)
  }

  def parse(tokens: Seq[String], prevTokens: Seq[Claus] = Nil)(implicit rule: MasterRule): Satze = 
    tokens match {
      case s :: others if (excludedTokens.contains(s.toLowerCase)) =>
        parse(others, prevTokens)

      case s :: others => 
        if (ModalVerb.isInstance(s)) {
          parseModalVerb(prevTokens, others, s)
        }
        else if (Verb.isInstance(s)) {
          parseVerb(prevTokens, others, s)
        }
        // Time has to be examined before preposition
        // since there are some overlapping 
        else if (Time.isInstance(s)){
          parseTime(prevTokens, others, s)
        }
        else if (Preposition.isInstance(s)) {
          parsePreposition(prevTokens, others, s)
        }
        else if (Artikel.isInstance(s)) {
          parseArtikel(prevTokens, others, s)
        }
        else if (Pronoun.isInstance(s)) {
          parsePronoun(prevTokens, others, s)
        }
        else if (Connector.isInstance(s)) {
          parseConnector(prevTokens, others, s)
        }
        else if (Negation.isInstance(s)) {
          parseNegation(prevTokens, others, s)
        }
        else if (Adj.isInstance(s)) {
          parseAdj(prevTokens, others, s)
        }
        else if (Adv.isInstance(s)) {
          parseAdv(prevTokens, others, s)
        }
        else {
          prevTokens match {
            case _ :+ TimeClaus(_,_) => parseTime(prevTokens, others, s)
            case _ => 
              println(YELLOW_B + "Unknown token : " + RESET + RED + s + RESET)
              parse(others, prevTokens)
          }
        }
      case Nil => Satze.from(prevTokens)
    }
}

