package com.starcolon.satze

import Console._
import com.starcolon.satze.Implicits._

trait Claus {
  def render(satze: Satze, index: Int)(implicit rule: MasterRule): String = toString
}

case object EmptyClaus extends Claus

sealed trait PronounClaus {
  val p: Pronoun
  val artikel: Artikel
  
  protected def renderSache(c: Case)(implicit rule: MasterRule) = 
    artikel.renderWith(rule.sache.findGender(p.s.capInitial), c) + " " + (artikel match {
      case Plural => rule.sache.findPlural(p.s.capInitial).capInitial
      case _ => p.s.capInitial
    })

  protected def renderInfinitiv(c: Case) = c match {
    case Nominativ => p.s
    case Akkusativ => p.akkusativ
    case Dativ => p.dativ
  }
}

case class SubjectClaus(
  override val artikel: Artikel = Ein,
  override val p: Pronoun = NP
) 
extends Claus 
with PronounClaus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule) = Pronoun.isInfinitiv(p) match {
    case true => renderInfinitiv(Nominativ)
    case false => renderSache(Nominativ)
  }
  override def toString = s"-${CYAN_B}S${RESET}:${artikel.toString} ${p.s.capInitial}"
}

case class VerbClaus(v: Verb) extends Claus {
  override def render(satze: Satze, index: Int)(implicit rule: MasterRule) = satze.subject match {
    case SubjectClaus(artikel, p) => rule.conjugation.conjugateVerb(v.v, p, artikel)
  }
  override def toString = s"-${YELLOW_B}V${RESET}:${v.v}"
}

case class ObjectClaus(
  val prep: Option[Preposition] = None,
  override val artikel: Artikel = Ein,
  override val p: Pronoun = NP
) 
extends Claus 
with PronounClaus {

  def hasPrep = prep.isDefined
  def hasArtikel = artikel != NoArtikel
  def hasPronoun = p != NP

  private def renderSoleObject(satze: Satze, masterCase: Option[Case])(implicit rule: MasterRule) = {
    
    prep.map(_.s + " ").getOrElse("") + (satze.verb match {
      case VerbClaus(v) => 
        val effectiveCase = masterCase.getOrElse(if (v.isAkkusativ) Akkusativ else Nominativ)
        Pronoun.isInfinitiv(p) match {
          case true => renderInfinitiv(effectiveCase)
          case false => renderSache(effectiveCase)
        }
    })
  }

  private def renderMultiObjects(
    satze: Satze, 
    masterCase: Option[Case], 
    index: Int,
    objects: Seq[(ObjectClaus, Int)])
  (implicit rule: MasterRule) = {

    // - Identify which is direct / indirect objects
    val indexes = objects.map(_._2)
    val isHead = indexes.head == index

    // __ + [this] + prep + other
    if (isHead && objects.last._1.hasPrep) {
      // direct object (akkusativ)
      renderSoleObject(satze, Some(Akkusativ))
    }
    // ___ + [this] + other
    else if (isHead){
      // indirect object (dativ)
      renderSoleObject(satze, Some(Dativ))
    }
    // ___ + other + [this]
    else {
      // direct object (akkusativ)
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

  override def toString = s"-${CYAN_B}O${RESET}:${prep.map(_.s).getOrElse("")} ${artikel.toString} ${p.s}"
}