package com.starcolon.satze

import Console._
import com.starcolon.satze.Implicits._

trait Claus {
  def render(satze: Satze, index: Int)(implicit rule: MasterRule): String = toString
}

sealed trait NegateClaus extends Claus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule): String = "nicht" 
  override def toString = "nicht"
}

case object NegateClaus extends NegateClaus

sealed trait PronounClaus {
  val ps: Seq[(Artikel,Adj,Pronoun)]
  val connector: Connector

  def isPlural = ps match {
    case ((a,j,p)) :: Nil => (a,p) match {
      case (Plural,_) => true
      case (_,Wir) => true
      case _ => false
    }

    case _ => true
  }

  def isPositional = ps match {
    case ((a,j,p)) :: Nil => 
      p.isInstanceOf[PositionalPronoun]

    case _ => false
  }
  
  protected def renderSache(c: Case)(pair: (Artikel, Adj, Pronoun))
  (implicit rule: MasterRule) = {
    val (a,j,p) = pair
    val gender = rule.sache.findGender(p.s.capInitial)
    val stringArtikel = a.renderWith(gender, c)
    val stringAdj     = j.renderWith(gender, c, a)
    val stringPronoun = a match {
      case Plural => rule.sache.findPlural(p.s.capInitial).capInitial
      case _ => p.s.capInitial
    }

    Seq(stringArtikel, stringAdj, stringPronoun).filterNot(_.isEmpty).mkString(" ").trim
  }
    
  protected def renderInfinitiv(c: Case)(pair: (Artikel, Adj, Pronoun)) = { 
    val (a,j,p) = pair
    (c match {
      case Nominativ => Seq(j.nominativ, p.s).mkString
      case Akkusativ => Seq(j.akkusativ, p.akkusativ).mkString
      case Dativ => Seq(j.dativ, p.dativ).mkString
    }).trim
  }

  protected def psToString = ps.map{ case(a,j,p) => 
    Seq(a.toString, j.toString, p.s.capInitial).mkString(" ")
  }.mkString(connector.s)
}

case class SubjectClaus(
  override val ps: Seq[(Artikel,Adj,Pronoun)] = Nil,
  override val connector: Connector = Und
) 
extends Claus 
with PronounClaus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule) = 
    ps.map{ case (a,j,p) =>
      Pronoun.isInfinitiv(p) match {
        case true => renderInfinitiv(Nominativ)((a,j,p))
        case false => renderSache(Nominativ)((a,j,p))
      }
    }.mkString(connector.s)

  override def toString = s"+ ${CYAN_B + BLACK}S${RESET}:${psToString}"
}

case class PrefixVerbClaus(v: Verb)
extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = {
    implicit val conjugation = rule.conjugation
    v.prefix
  }
}

case class VerbClaus(v: Verb) 
extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = {
    implicit val conjugation = rule.conjugation

    implicit val satze_ = satze
    lazy val subj = satze.subject.get
    lazy val obj = satze.objekt

    val (a,j,p) = if (isPluralVerb) (NoArtikel, Adj(Nil), Wir) else subj.ps.head

    if (satze.isPerfekt){
      rule.conjugation.conjugatePerfektVerb(v.v, p)._2
    }
    else {
      // Present tense
      if (isAfterObjekt){
        rule.conjugation.conjugateVerb(v.v, p, a)
      }
      else {
        rule.conjugation.conjugateVerb(v.v, p, a).ohnePrefix
      }
    }

    // if (satze.isPerfekt){
    //   // Perfekt tense
    //   val (_, perfektVerb) = if (subj.isPlural || 
    //       (subj.isPositional && obj.map(_.isPlural).getOrElse(false))) {
    //     rule.conjugation.conjugatePerfektVerb(v.v, Wir)
    //   }
    //   else { 
    //     val (a,j,p) = subj.ps.head
    //     rule.conjugation.conjugatePerfektVerb(v.v, p)
    //   }

    //   perfektVerb
    // }
    // else {
    //   // Present tense
    //   if (subj.isPlural || 
    //       (subj.isPositional && obj.map(_.isPlural).getOrElse(false))) {
    //     rule.conjugation.conjugateVerb(v.v, Wir, NoArtikel).ohnePrefix
    //   }
    //   else { 
    //     val (a,j,p) = subj.ps.head
    //     rule.conjugation.conjugateVerb(v.v, p, a).ohnePrefix
    //   }
    // }
  }

  def prefix = PrefixVerbClaus(v)

  def isAfterObjekt(implicit satze: Satze) = satze.isAfter[VerbClaus, ObjectClaus]

  def isPluralVerb(implicit satze: Satze) = {
    lazy val subj = satze.subject.get
    lazy val obj = satze.objekt
    lazy val verb = satze.verb.get

    if (subj.isPlural) true
    else if (verb.v.v == "seid" && obj.map{_.isPlural}.getOrElse(false)) true
    else false
  }

  def isSeparable(implicit rule: MasterRule) = {
    rule.conjugation.isSeparable(v.v)
  }

  override def toString = s"+ ${YELLOW_B + BLACK}V${RESET}:${v.v}"

  def isHaben()(implicit rule: MasterRule) = rule.conjugation.deconjugateVerb(v.v) == "haben"
}

trait HabenVerbClaus extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = {
    implicit val conjugation = rule.conjugation

    val subj = satze.subject.get
    val obj = satze.objekt
    val verb = satze.verb.get

    val (habenVerb, _) = if (subj.isPlural || 
        (subj.isPositional && obj.map(_.isPlural).getOrElse(false))) {
      conjugation.conjugatePerfektVerb(verb.v.v, Wir)
    }
    else { 
      val (a,j,p) = subj.ps.head
      conjugation.conjugatePerfektVerb(verb.v.v, p)
    }

    habenVerb
  }

  override def toString = s"${GREEN_B + BLACK}Seid/Haben${RESET}"
}

case object HabenVerbClaus extends HabenVerbClaus

case class ModalVerbClaus(v: ModalVerb)
extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = satze.subject.get match {
    case SubjectClaus(ps,_) => ps match {
      // Single subject, and modal verb is not placed at the end
      case ((a,j,p)) :: Nil if index < satze.clauses.size-1 => 
        rule.conjugation.conjugateVerb(v.v, p, a)

      // Multiple subjects
      case _ =>
        rule.conjugation.conjugateVerb(v.v, Wir, NoArtikel)
    }
  }

  override def toString = s"+ ${GREEN_B + BLACK}MV${RESET}:${v.v}"
}

case class ObjectClaus(
  val prep: Option[Preposition] = None,
  override val ps: Seq[(Artikel,Adj,Pronoun)] = Nil,
  override val connector: Connector = Space
) 
extends Claus 
with PronounClaus {

  private def renderSoleObject(satze: Satze, masterCase: Option[Case])
  (implicit rule: MasterRule) = {
    
    prep.map(_.s + " ").getOrElse("") + (satze.verb.get match {
      case VerbClaus(v) => 
        val effectiveCase = masterCase.getOrElse(
          if (v.isAkkusativ) Akkusativ 
          else Nominativ
        )
        ps.map { case (a,j,p) => 
          Pronoun.isInfinitiv(p) match {
            case true => renderInfinitiv(effectiveCase)((a,j,p))
            case false => renderSache(effectiveCase)((a,j,p))
          }
        }.mkString(connector.s)
    })
  }

  private def renderMultiObjects(
    satze: Satze, 
    masterCase: Option[Case], 
    index: Int,
    objects: Seq[(ObjectClaus, Int)])
  (implicit rule: MasterRule) = {

    // - Identify which is direct / indirect objects
    val (objs, indexes) = objects.unzip
    val isFirstObject = indexes.head == index

    if (isFirstObject){
      
      objs.last.prep match {
        
        // __ + [this] + prep + other
        // This is an indirect object
        case None => renderSoleObject(satze, Some(Dativ))
          
        // __ + [this] + other
        // This is a direct object
        case _ => renderSoleObject(satze, Some(Akkusativ))
      }
    }
    else {
      // Not the first object,
      // Direct object (akkusativ)
      renderSoleObject(satze, Some(Akkusativ))
    }
  }

  override def render(satze: Satze, index: Int)(implicit rule: MasterRule) = {
    val objects = satze.clauses
      .zipWithIndex
      .collect{ 
        case(c: ObjectClaus, i) =>
          (c.asInstanceOf[ObjectClaus], i)
        }
    val VerbClaus(v) = satze.verb.get
    val masterCase = prep.map(_.getCase(v))
    
    // Multiple objects 
    // & Current object does not have a preposition
    if (prep == None && objects.size > 1){
      renderMultiObjects(satze, masterCase, index, objects)
    }
    else {
      // Single object, or with preposition
      renderSoleObject(satze, masterCase)
    }
  }

  override def toString = s"-${CYAN_B + BLACK}O${RESET}:${prep.map(_.s).getOrElse("")} ${psToString}"
}

case class TimeClaus(am: Option[Am], um: Option[Um])
extends Claus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule) = {
    Seq(am, um).map(_.map(_.toString).getOrElse("")).mkString(" ").trim
  }

  override def toString = s"-${RED_B + BLACK}T${RESET}:am=${am},um=${um}"
}