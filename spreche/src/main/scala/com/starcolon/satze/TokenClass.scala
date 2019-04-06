package com.starcolon.satze

import com.starcolon.satze.Implicits._

// Traits

sealed trait Token
case object NoToken extends Token

sealed trait Artikel extends Token {
  def renderWith(gender: String, c: Case): String
  def matchWith(s: String): Boolean
  override def toString = this.getClass.getName.dropRight(1).split('.').last
}

sealed trait Pronoun extends Token {
  val s: String
  val dativ: String 
  val akkusativ: String
  val possess: String
}

sealed trait TokenInstance {
  def isInstance(token: String)(implicit rule: MasterRule): Boolean
}


// Classes and Objects

case class Verb(v: String) extends Token {
  def isAkkusativ: Boolean = v != "sein"
  override def toString = v
}

object Verb extends TokenInstance {
  /**
   * Parse a string to verb (deconjugated)
   */
  def toVerb(s: String)(implicit rule: ConjugationRule): Verb = {
    Verb(rule.deconjugateVerb(s))
  }

  override def isInstance(token: String)(implicit rule: MasterRule) = 
    rule.conjugation.isVerb(token.toLowerCase)
}

case class ModalVerb(v: String) extends Token {
  override def toString = v
  def render(subject: SubjectClaus)(implicit rule: MasterRule) = {
    rule.conjugation.conjugateVerb(
      v, subject.p, subject.artikel
    )
  }
}

object ModalVerb extends TokenInstance {
  def getList = Seq(
    "möchten",
    "wollen",
    "sollen",
    "müssen",
    "können",
    "dürfen"
  )

  override def isInstance(token: String)(implicit rule: MasterRule) =  
    rule.conjugation.isVerb(token.toLowerCase) && ModalVerb.getList.contains(
      rule.conjugation.deconjugateVerb(token.toLowerCase)
    )

  /**
   * Parse a string to verb (deconjugated)
   */
  def toVerb(s: String)(implicit rule: ConjugationRule): ModalVerb = {
    ModalVerb(rule.deconjugateVerb(s))
  }
}

case class Preposition(s: String) extends Token {
  def getCase(v: Verb) = v match {
    case Verb("gehen") | Verb("kommen") => Akkusativ 
    case Verb("sein") => Akkusativ
    case _ => Preposition.getCase(s)
  }
}

object Preposition extends TokenInstance {
  def getCase(s: String) = PrepositionRule.mapCase.getOrElse(s, Nominativ)
  override def isInstance(token: String)(implicit rule: MasterRule) =
    PrepositionRule.isPreposition(token)
}


case object NoArtikel extends Artikel {
  override def matchWith(s: String) = s.trim.isEmpty
  override def renderWith(gender: String, c: Case) = ""
}

case object Ein extends Artikel {
  override def matchWith(s: String) = {
    Seq("ein","eine","einer","einem","einen").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "ein"
      case "die" => "eine"
      case "das" => "ein"
      case _ => ""
    }
    case Akkusativ => gender match {
      case "der" => "einen"
      case "die" => "eine"
      case "das" => "ein"
      case _ => ""
    }
    case Dativ => gender match {
      case "der" => "einem"
      case "die" => "einer"
      case "das" => "einem"
      case _ => ""
    }
  }
}

case object Der extends Artikel {
  override def matchWith(s: String) = {
    Seq("der","die","das","den","dem").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "der"
      case "die" => "die"
      case "das" => "das"
    }
    case Akkusativ => gender match {
      case "der" => "den"
      case "die" => "die"
      case "das" => "das"
    }
    case Dativ => gender match {
      case "der" => "dem"
      case "die" => "der"
      case "das" => "dem"
    }
  }
}

case object Plural extends Artikel {
  override def matchWith(s: String) = {
    Seq("viel","viele").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => "viele"
    case Akkusativ => "die"
    case Dativ => "den"
  }

  override def toString = "Plural"
}

case object Kein extends Artikel {
  override def matchWith(s: String) = {
    Seq("kein","keine","keiner","keinem","keinen").contains(s.toLowerCase)
  }
  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "kein"
      case "die" => "keine"
      case "das" => "kein"
    }
    case Akkusativ => gender match {
      case "der" => "keinen"
      case "die" => "keine"
      case "das" => "kein"
    }
    case Dativ => gender match {
      case "der" => "keinem"
      case "die" => "keiner"
      case "das" => "keinem"
    }
  }
}

case object Mein extends Artikel {
  override def matchWith(s: String) = {
    Seq("mein","meine","meiner","meinem","meinen").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "mein"
      case "die" => "meine"
      case "das" => "mein"
    }
    case Akkusativ => gender match {
      case "der" => "meinen"
      case "die" => "meine"
      case "das" => "mein"
    }
    case Dativ => gender match {
      case "der" => "meinem"
      case "die" => "meine"
      case "das" => "meinem"
    }
  }
}

case object Dein extends Artikel {
  override def matchWith(s: String) = {
    Seq("dein","deine","deiner","deinem","deinen").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "dein"
      case "die" => "deine"
      case "das" => "dein"
    }
    case Akkusativ => gender match {
      case "der" => "deinen"
      case "die" => "deine"
      case "das" => "dein"
    }
    case Dativ => gender match {
      case "der" => "deinem"
      case "die" => "deine"
      case "das" => "deinem"
    }
  }
}

case object Sein extends Artikel {
  override def matchWith(s: String) = {
    Seq("sein","seine","seiner","seinem","seinen").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "sein"
      case "die" => "seine"
      case "das" => "sein"
    }
    case Akkusativ => gender match {
      case "der" => "seinen"
      case "die" => "seine"
      case "das" => "sein"
    }
    case Dativ => gender match {
      case "der" => "seinem"
      case "die" => "seine"
      case "das" => "seinem"
    }
  }
}

case object Ihre extends Artikel {
  override def matchWith(s: String) = {
    Seq("ihr","ihre","ihren","ihrem").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "ihr"
      case "die" => "ihre"
      case "das" => "ihr"
    }
    case Akkusativ => gender match {
      case "der" => "ihren"
      case "die" => "ihre"
      case "das" => "ihr"
    }
    case Dativ => gender match {
      case "der" => "ihrem"
      case "die" => "ihre"
      case "das" => "ihrem"
    }
  }
}

case object Unser extends Artikel {
  override def matchWith(s: String) = {
    Seq("unser","unsere","unseren","unserem").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "unser"
      case "die" => "unsere"
      case "das" => "unser"
    }
    case Akkusativ => gender match {
      case "der" => "unseren"
      case "die" => "unsere"
      case "das" => "unser"
    }
    case Dativ => gender match {
      case "der" => "unserem"
      case "die" => "unser"
      case "das" => "unserem"
    }
  }
}

case object Euer extends Artikel {
  override def matchWith(s: String) = {
    Seq("eure","euer","euren","eurem").contains(s.toLowerCase)
  }

  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "euer"
      case "die" => "eure"
      case "das" => "euer"
    }
    case Akkusativ => gender match {
      case "der" => "euren"
      case "die" => "eure"
      case "das" => "euren"
    }
    case Dativ => gender match {
      case "der" => "eurem"
      case "die" => "eure"
      case "das" => "eurem"
    }
  }
}

object Artikel extends TokenInstance {
  
  def toArtikel(s: String)(implicit rule: MasterRule): Artikel = {
    Seq(Ein,Der,Plural,Kein,Mein,Dein,Sein,Unser,Ihre).find(_.matchWith(s)).getOrElse(Ein)
  }

  override def isInstance(token: String)(implicit rule: MasterRule) = {
    val artikels = (Seq("die","das","eine","ein","kein","keine") ++ Seq("d","ein","kein").flatMap{(a) => 
      Seq("er","en","em").map(a + _)
    }) ++ Seq("viel", "viele")
    def expand(p: Pronoun) = Seq("","e","en","em").map(p.possess + _)
    val possArtikels = Pronoun.infinitivPronouns.flatMap(expand)
    artikels.contains(token.toLowerCase) || possArtikels.contains(token.toLowerCase)
  }
}


case object NP extends Pronoun {
  override val s = ""
  override val akkusativ = ""
  override val dativ = ""
  override val possess = ""
}
case object Ich extends Pronoun{
  override val s = "ich"
  override val akkusativ = "mich"
  override val dativ = "mir"
  override val possess = "mein"
}
case object Du extends Pronoun{
  override val s = "du"
  override val akkusativ = "dich"
  override val dativ = "dir"
  override val possess = "dein"
}
case object Sie extends Pronoun{
  override val s = "sie"
  override val akkusativ = "sie"
  override val dativ = "ihr"
  override val possess = "ihr"
}
case object Er extends Pronoun{
  override val s = "er"
  override val akkusativ = "ihn"
  override val dativ = "ihm"
  override val possess = "seid"
}
case object Es extends Pronoun{
  override val s = "es"
  override val akkusativ = "es"
  override val dativ = "es"
  override val possess = "ihr"
}
case object Wir extends Pronoun{
  override val s = "wir"
  override val akkusativ = "uns"
  override val dativ = "uns"
  override val possess = "unser"
}
case object Ihr extends Pronoun{
  override val s = "ihr"
  override val akkusativ = "euch"
  override val dativ = "euch"
  override val possess = "euer"
}

case class Instance(override val s: String) extends Pronoun {
  override val akkusativ = s
  override val dativ = s
  override val possess = ""
}

object Pronoun extends TokenInstance {
  lazy val infinitivPronouns = List(Ich, Du, Sie, Wir, Ihr, Er, Es)
  lazy val reverseMap = infinitivPronouns.flatMap{ p => 
    Seq((p.s, p.s), (p.akkusativ, p.s), (p.dativ, p.s))
  }.toMap
  private lazy val infinitivPronounStrings = infinitivPronouns.flatMap{ p => Seq(p.s, p.akkusativ, p.dativ)}
  private lazy val map = infinitivPronouns.map(p => (p.s, p)).toMap
  
  def isInfinitiv(s: String) = infinitivPronounStrings.contains(s)
  def isInfinitiv(p: Pronoun) = infinitivPronouns.contains(p)
  def toPronoun(s: String) = {
    val s_ = reverseMap.getOrElse(s, s)
    map.getOrElse(s_, Instance(s_))
  }

  override def isInstance(token: String)(implicit rule: MasterRule) = {
    Pronoun.isInfinitiv(token.toLowerCase) || 
    rule.sache.isSache(token.capInitial)
  }
}
