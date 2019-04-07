package com.starcolon.satze

import Console._
import com.starcolon.satze.Implicits._

trait Claus {
  def render(satze: Satze, index: Int)(implicit rule: MasterRule): String = toString
}

case object EmptyClaus extends Claus

sealed trait PronounClaus {
  val ps: Seq[(Artikel,Pronoun)]
  val connector: Connector
  
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
  (implicit rule: MasterRule) = satze.subject match {
    case SubjectClaus(ps,_) => ps match {
      // Single subject
      case ((a,p)) :: Nil =>
        rule.conjugation.conjugateVerb(v.v, p, a)

      // Multiple subjects
      case _ =>
        rule.conjugation.conjugateVerb(v.v, Wir, NoArtikel)
    }
  }
  
  override def toString = s"+ ${YELLOW_B + BLACK}V${RESET}:${v.v}"
}

case class ModalVerbClaus(v: ModalVerb)
extends Claus {
  override def render(satze: Satze, index: Int)
  (implicit rule: MasterRule) = satze.subject match {
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
  override val connector: Connector = Und
) 
extends Claus 
with PronounClaus {

  private def renderSoleObject(satze: Satze, masterCase: Option[Case])
  (implicit rule: MasterRule) = {
    
    prep.map(_.s + " ").getOrElse("") + (satze.verb match {
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
        // This is a direct object
        case None => renderSoleObject(satze, Some(Akkusativ))
          
        // __ + [this] + other
        // Another is a direct object
        case _ => renderSoleObject(satze, Some(Dativ))
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
      .filter(_._1.isInstanceOf[ObjectClaus])
      .map{ case(c,i) => (c.asInstanceOf[ObjectClaus], i)}
    val VerbClaus(v) = satze.verb.asInstanceOf[VerbClaus]
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