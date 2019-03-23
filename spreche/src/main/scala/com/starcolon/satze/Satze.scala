package com.starcolon.satze

import com.starcolon.satze._
import com.starcolon.satze.Implicits._

trait Claus {
  def render(prev: Claus, next: Claus)(implicit rule: MasterRule): String = toString
}

case object EmptyClaus extends Claus

case class SubjectClaus(p: Pronoun, adj: Option[String] = None) extends Claus {
  override def render(prev: Claus, next: Claus)(implicit rule: MasterRule) = p.toString
}

case class VerbClaus(v: Verb) extends Claus {
  override def render(prev: Claus, next: Claus)(implicit rule: MasterRule) = prev match {
    case SubjectClaus(p, _) => rule.conjugation.conjugateVerb(v.v, p)
  }
}

case class ObjectClaus(directNoun: Pronoun, dativNoun: Option[Pronoun] = None, artikel: Artikel = Ein) extends Claus {
  private def renderInfinitiv(c: Case) = c match {
    case Nominativ => directNoun.s
    case Akkusativ => directNoun.akkusativ
    case Dativ => directNoun.dativ
  }

  private def renderSache(c: Case)(implicit rule: MasterRule) = 
    artikel.renderWith(rule.sache.findGender(directNoun.s), c) + " " + directNoun.s

  override def render(prev: Claus, next: Claus)(implicit rule: MasterRule) = {
      prev match {
        case VerbClaus(v) => Pronoun.isInfinitiv(directNoun) match {
          case true => renderInfinitiv(if (v.isAkkusativ) Akkusativ else Nominativ)
          case false => renderSache(if (v.isAkkusativ) Akkusativ else Nominativ)
        }
    }
  }
}

class Satze(clauses: Seq[Claus]) extends Claus {

  def isVerb(token: String)(implicit rule: MasterRule) = {
    rule.conjugation.isVerb(token.toLowerCase)
  }

  def isPronoun(token: String)(implicit rule: MasterRule) = {
    if (Pronoun.isInfinitiv(token.toLowerCase))
      true
    else
      rule.sache.isSache(token.capInitial)
  }

  def parse(tokens: Seq[String], prevTokens: Seq[Claus] = Nil)(implicit rule: MasterRule): Satze = {
    // The sentence can either begin with a pronoun or a verb (Frage)

    ???
  }

  override def render(prev: Claus, next: Claus)(implicit rule: MasterRule) = ???
}

