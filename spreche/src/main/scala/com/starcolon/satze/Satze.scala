package com.starcolon.satze

import com.starcolon.satze._
import com.starcolon.satze.Implicits._
import Console._

case class Satze(clauses: Seq[Claus]) extends Claus {
  def subject: Option[SubjectClaus]     = clauses.find(_.isInstanceOf[SubjectClaus]).map(_.asInstanceOf[SubjectClaus])
  def verb: Option[VerbClaus]           = clauses.find(_.isInstanceOf[VerbClaus]).map(_.asInstanceOf[VerbClaus])
  def modalVerb: Option[ModalVerbClaus] = clauses.find(_.isInstanceOf[ModalVerbClaus]).map(_.asInstanceOf[ModalVerbClaus])
  def objekt: Option[ObjectClaus]       = clauses.find(_.isInstanceOf[ObjectClaus]).map(_.asInstanceOf[ObjectClaus])
  def time: Option[TimeClaus]           = clauses.find(_.isInstanceOf[TimeClaus]).map(_.asInstanceOf[TimeClaus])
  
  override def render(satze: Satze = this, index: Int = -1)(implicit rule: MasterRule) = {
    
    // Render time at the beginning of the satze (if any)
    val timePrefix = time.map(_.render(satze,-1) + " ").getOrElse("")
    
    timePrefix + (modalVerb match {
      // Without modal verb
      case None => renderSVO()
      case Some(ModalVerbClaus(_)) => 
        // Modal verb but without verb, 
        // modal verb will become a verb itself
        verb match {
          
          case None => 
            Satze(clauses.map{
              case ModalVerbClaus(v) => VerbClaus(v.toVerb)
              case any => any
            }).render(this)

          case _ => renderSMOV()
        }
    })
  }

  private def renderSMOV()
  (implicit rule: MasterRule) = {
    
    val putModalVerbInFront = time.isDefined
    val verbClaus = verb.get

    // Put modal verb in front if needed
    val clausesToRender = if (putModalVerbInFront)
        clauses.filterNot{ c =>
        c.isInstanceOf[VerbClaus] ||
        c.isInstanceOf[TimeClaus] ||
        c.isInstanceOf[ModalVerbClaus]
      }
      else
        clauses.filterNot{ c =>
          c.isInstanceOf[VerbClaus] ||
          c.isInstanceOf[TimeClaus]
        }
    val beginning = if (putModalVerbInFront){
      modalVerb.map(_.render(this, -1)).getOrElse("") + " "
    }
    else ""

    val endingVerb = rule.conjugation.conjugateVerb(
      verbClaus.v.v, Wir, NoArtikel
    )

    val isNegate = clausesToRender.headOption == Some(NegateClaus)
    val preVerbToken = if (isNegate) " nicht " else " "
    
    beginning + Satze.abbrev(clausesToRender.zipWithIndex.map{
      case(c,i) => c.render(this, i).trim
    }.mkString(" ")) + preVerbToken + endingVerb
  }

  private def renderSVO()(implicit rule: MasterRule) = {
    val putVerbInFront = time.isDefined
    val (clausesToRender, beginning): (Seq[Claus], String) =
      if (!putVerbInFront)
        (clauses, "")
      else{
        val clausesNoVerb: Seq[Claus] = clauses.filterNot(_.isInstanceOf[VerbClaus])
        val verbStr = clauses.filter(_.isInstanceOf[VerbClaus])
          .headOption
          // NOTE: Following render has to supply the full sentence with verbs
          .map(_.render(Satze(clauses),-1) + " ")
          .getOrElse("")

        (clausesNoVerb, verbStr)
      }
    val satzeRef = this
    
    val isNegate = clausesToRender.headOption == Some(NegateClaus)
    val ending = if (isNegate) " nicht" else ""

    beginning + Satze.abbrev(clausesToRender
      .filterNot(_.isInstanceOf[TimeClaus])
      .zipWithIndex.map{
        case(c,i) => c.render(satzeRef, i).trim
      }.mkString(" ")) + ending
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

  def from(clauses: Seq[Claus]) = {

    def correctIhrDas(ps: Seq[(Artikel,Pronoun)]) = ps.map{ 
      case (Ihre,NP) => (NoArtikel,Ihr)
      case (Der,NP) => (NoArtikel,Das)
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
          ns :+ ObjectClaus(prep, ps0 :+ ((a0,p)), c)

        // _ + P + [P]
        case _ => c match {
          // 2nd object
          // _ + artikel + P + [P]
          case Space =>
            prevTokens :+ ObjectClaus(None, ((Ein,p)) :: Nil, c)

          // Multiple objects
          // _ + P + und + [P]
          case _ =>
            ns :+ ObjectClaus(prep, ps :+ ((Ein,p)), c)
        }
      }
      
      // _ + [P]
      case _ => 
        prevTokens :+ ObjectClaus(ps = (Ein,p) :: Nil)
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

