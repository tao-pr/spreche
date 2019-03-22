package com.starcolon.satze

import com.starcolon.satze.{Rule, FrageWort}
import com.starcolon.satze.{Wie, Wo, Warum, Wann, Wer}

trait Claus {
  def render(prev: Claus, next: Claus)(implicit val rule: MasterRule): String = toString
}

case object EmptyClaus extends Claus

case class SubjectClaus(p: Pronoun, adj: Option[String] = None) extends Claus {
  override def render(prev: Claus, next: Claus)(implicit val rule: MasterRule) = p.toString
}

case class VerbClaus(v: Verb) extends Claus {
  override def render(prev: Claus, next: Claus)(implicit val rule: MasterRule) = prev match {
    case SubjectClaus(p, _) => rule.conjugation.conjugateVerb(v.s, p)
  }
}

case class ObjectClaus(directNoun: Pronoun, dativNoun: Option[Pronoun] = None, artikel: Artikel = Ein) extends Claus {
  private def renderInfinitiv(c: Case) = c match {
    case Nominativ => directNoun.s
    case Akkusativ => directNoun.akkusativ
    case Dativ => directNoun.dativ
  }

  private def renderSache(c: Case)(implicit val rule: MasterRule) = 
    artikel.renderWith(rule.sache.findGender(directNoun.s), c) + " " + directNoun.s

  override def render(prev: Claus, next: Claus)(implicit val rule: MasterRule) = {
      prev match {
        case VerbClaus(v) => Pronoun.isInfinitiv(directNoun) match {
          case true => renderInfinitiv(Akkusativ if v.isAkkusativ else Nominativ)
          case false => renderSache(Akkusativ if v.isAkkusativ else Nominativ)
        }
    }
  }
}

class Satze(clauses: Seq[Claus]) extends Claus {
  override def render(prev: Claus, next: Claus)(implicit val rule: MasterRule) = ???
}

