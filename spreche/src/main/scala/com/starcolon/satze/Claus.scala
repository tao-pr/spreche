package com.starcolon.satze

import Console._
import com.starcolon.satze.Implicits._

trait Claus {
  def render(satze: Satze)(implicit rule: MasterRule): String = toString
}

case object EmptyClaus extends Claus

sealed trait PronounClaus {
  val p: Pronoun
  val artikel: Artikel
  
  protected def renderSache(c: Case)(implicit rule: MasterRule) = 
    artikel.renderWith(rule.sache.findGender(p.s.capInitial), c) + " " + p.s.capInitial

  protected def renderInfinitiv(c: Case) = c match {
    case Nominativ => p.s
    case Akkusativ => p.akkusativ
    case Dativ => p.dativ
  }
}

case class SubjectClaus(override val p: Pronoun, adj: Option[String] = None, override val artikel: Artikel = Ein) 
extends Claus 
with PronounClaus {
  override def render(satze: Satze)(implicit rule: MasterRule) = Pronoun.isInfinitiv(p) match {
    case true => renderInfinitiv(Nominativ)
    case false => renderSache(Nominativ)
  }
  override def toString = s"-${CYAN_B}S${RESET}:${p.s.capInitial}"
}

case class VerbClaus(v: Verb) extends Claus {
  override def render(satze: Satze)(implicit rule: MasterRule) = satze.subject match {
    case SubjectClaus(p, _, _) => rule.conjugation.conjugateVerb(v.v, p)
  }
  override def toString = s"-${YELLOW_B}V${RESET}:${v.v}"
}

case class ObjectClaus(override val p: Pronoun, dativNoun: Option[Pronoun] = None, override val artikel: Artikel = Ein) 
extends Claus 
with PronounClaus {

  override def render(satze: Satze)(implicit rule: MasterRule) = {
      satze.verb match {
        case VerbClaus(v) => Pronoun.isInfinitiv(p) match {
          case true => renderInfinitiv(if (v.isAkkusativ) Akkusativ else Nominativ)
          case false => renderSache(if (v.isAkkusativ) Akkusativ else Nominativ)
        }
    }
  }

  override def toString = s"-${CYAN_B}O${RESET}:${artikel.toString} ${p.s}"
}