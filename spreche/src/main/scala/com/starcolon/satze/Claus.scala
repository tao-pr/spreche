package com.starcolon.satze

import Console._
import com.starcolon.satze.Implicits._

trait Claus {
  def render(satze: Satze, index: Int)(implicit rule: MasterRule): String = toString
}

case object NegateClaus extends Claus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule): String = "" 
  override def toString = "nicht"
}

sealed trait PronounClaus {
  val ps: Seq[(Artikel,Pronoun)]
  val connector: Connector

  def isPlural = ps match {
    case ((a,p)) :: Nil => a match {
      case Plural => true
      case _ => false
    }

    case _ => true
  }

  def isPositional = ps match {
    case ((a,p)) :: Nil => 
      p.isInstanceOf[PositionalPronoun]

    case _ => false
  }
  
  protected def renderSache(c: Case)(pair: (Artikel, Pronoun))
  (implicit rule: MasterRule) = {
    val (a,p) = pair
    a.renderWith(rule.sache.findGender(p.s.capInitial), c) + " " + (a match {
      case Plural => rule.sache.findPlural(p.s.capInitial).capInitial
      case _ => p.s.capInitial
    })
  }
    
  protected def renderInfinitiv(c: Case)(pair: (Artikel, Pronoun)) = { 
    val (a,p) = pair
    c match {
      case Nominativ => p.s
      case Akkusativ => p.akkusativ
      case Dativ => p.dativ
    }
  }

  protected def psToString = ps.map{ case(a,p) => 
    a.toString + " " + p.s.capInitial
  }.mkString(connector.s)
}

case class SubjectClaus(
  override val ps: Seq[(Artikel,Pronoun)] = Nil,
  override val connector: Connector = Und
) 
extends Claus 
with PronounClaus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule) = 
    ps.map{ case (a,p) =>
      Pronoun.isInfinitiv(p) match {
        case true => renderInfinitiv(Nominativ)((a,p))
        case false => renderSache(Nominativ)((a,p))
      }
    }.mkString(connector.s)

  override def toString = s"+ ${CYAN_B + BLACK}S${RESET}:${psToString}"
}

case class VerbClaus(v: Verb) 
extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = {
    val subj = satze.subject.get
    val obj = satze.objekt

    if (subj.isPlural || 
        (subj.isPositional && obj.map(_.isPlural).getOrElse(false))) {
      rule.conjugation.conjugateVerb(v.v, Wir, NoArtikel)
    }
    else { 
      val (a,p) = subj.ps.head
      rule.conjugation.conjugateVerb(v.v, p, a)
    }
  }

  override def toString = s"+ ${YELLOW_B + BLACK}V${RESET}:${v.v}"
}

case class ModalVerbClaus(v: ModalVerb)
extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = satze.subject.get match {
    case SubjectClaus(ps,_) => ps match {
      // Single subject
      case ((a,p)) :: Nil => 
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
  override val ps: Seq[(Artikel,Pronoun)] = Nil,
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
        ps.map { case (a,p) => 
          Pronoun.isInfinitiv(p) match {
            case true => renderInfinitiv(effectiveCase)((a,p))
            case false => renderSache(effectiveCase)((a,p))
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
      println(s"master case determined by preprosition: ${masterCase}")
      renderSoleObject(satze, masterCase)
    }
  }

  override def toString = s"-${CYAN_B + BLACK}O${RESET}:${prep.map(_.s).getOrElse("")} ${psToString}"
}