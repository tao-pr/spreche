package com.starcolon.satze

import com.starcolon.satze.Implicits._

// Traits

sealed trait Token
sealed trait Connector extends Token {
  def s: String
}

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

sealed trait Time extends Token {
  val t: String
}


// Classes and Objects

case class Adj(s: Seq[String]) extends Token {
  def nominativ: String = s.filterNot(_.isEmpty).mkString(" ")
  def akkusativ: String = s.filterNot(_.isEmpty).mkString(" ")
  def dativ: String = s.filterNot(_.isEmpty).mkString(" ")

  def renderWith(gender: String, c: Case, a: Artikel): String = s.map{ n => 
    c match {
      case Nominativ => (gender, a) match {
        case ("der",Ein) => n.ensureEnding("er")
        case ("das",Ein) => n.ensureEnding("es")
        case (_,Plural) => n.ensureEnding("en") // die
        case _ => n.ensureEnding("e")
      }

      case _ => (gender,c) match {
        case ("die",Akkusativ) => n.ensureEnding("e")
        case ("das",Akkusativ) if a==Der => n.ensureEnding("e")
        case ("das",Akkusativ) if a==Ein => n.ensureEnding("es")
        case _ => n.ensureEnding("en")
      }
    }
  }.mkString(" ")
}

case object Und extends Connector { override val s = " und " }
case object Oder extends Connector { override val s =" oder "}
case object Space extends Connector { override val s = " " }

object Connector extends TokenInstance {
  override def isInstance(token: String)(implicit rule: MasterRule) = 
    Seq("und","oder").contains(token.toLowerCase)

  def toConnector(s: String): Connector = s.toLowerCase.trim match {
    case "und" => Und
    case "oder" => Oder
    case _ => Space
  }
}

object Negation extends TokenInstance {
  override def isInstance(token: String)(implicit rule: MasterRule) = {
    token.toLowerCase == "nicht"
  }
}

case class Verb(v: String) extends Token {
  def isAkkusativ: Boolean = v != "seid"
  override def toString = v

  /**
   * Get separable prefix (if any)
   */
  def prefix()(implicit rule: ConjugationRule): String = 
    rule.separate(v).map{_._1}.getOrElse("")

  def ohnePrefix()(implicit rule: ConjugationRule): String = 
    v.ohnePrefix
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
  def toVerb = Verb(v)
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
    case Verb("sein") => Akkusativ
    case _ => Preposition.getCase(s)
  }
}

object Preposition extends TokenInstance {
  def getCase(s: String) = PrepositionRule.mapCase.getOrElse(
    PrepositionRule.generalise(s), 
    Nominativ)
  override def isInstance(token: String)(implicit rule: MasterRule) =
    PrepositionRule.isPreposition(token)
}


case object NoArtikel extends Artikel {
  override def matchWith(s: String) = s.trim.isEmpty
  override def renderWith(gender: String, c: Case) = ""
}

case object Diese extends Artikel {
  override def matchWith(s: String) = {
    Seq("diese","diesen","dieser","diesem").contains(s.toLowerCase)
  }
  override def renderWith(gender: String, c: Case) = c match {
    case Nominativ => gender match {
      case "der" => "dieser"
      case "die" => "diese"
      case "das" => "dieses"
      case _ => ""
    }
    case Akkusativ => gender match {
      case "der" => "diesen"
      case "die" => "diese"
      case "das" => "dieses"
      case _ => ""
    }
    case Dativ => gender match {
      case "der" => "diesem"
      case "die" => "diesen"
      case "das" => "diesem"
      case _ => ""
    }
  } 
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
    Seq(Ein,Der,Plural,Kein,Mein,Dein,Sein,Unser,Ihre,Euer,Diese).find(
      _.matchWith(s)
    ).getOrElse(Ein)
  }

  override def isInstance(token: String)(implicit rule: MasterRule) = {
    val artikels = (
      Seq("die","das","eine","ein","kein","keine","diese") ++ 
      Seq("d","ein","kein","dies").flatMap{(a) => 
        Seq("er","en","em").map(a + _)
      }) ++ Seq("viel", "viele","den","dem")
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

/**
 * Positional pronoun can be either singular or plural 
 * given the value of the counterpart
 */
trait PositionalPronoun extends Pronoun {
  override val akkusativ = s
  override val dativ = s
  override val possess = s
}
case object Das extends PositionalPronoun { override val s = "das" }
case object Da extends PositionalPronoun { override val s = "da" }
case object Dort extends PositionalPronoun { override val s = "dort" }

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
  override val possess = "sein"
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
  lazy val infinitivPronouns = List(Ich, Du, Sie, Wir, Ihr, Er, Es, Da, Dort, Das)
  lazy val reverseMap = infinitivPronouns.flatMap{ p => 
    Seq((p.s, p.s), (p.akkusativ, p.s), (p.dativ, p.s))
  }.toMap
  
  private lazy val infinitivPronounStrings = infinitivPronouns.flatMap{ p => 
    Seq(p.s, p.akkusativ, p.dativ)
  }.toSet

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

case class Um(override val t: String) extends Time {
  override def toString: String = {
    val timeTokens = t.split(":").toSeq
    val (prefix, suffix) = if (Time.allPredef.contains(t)) 
      ("","") else ("um "," Uhr ")
    timeTokens.headOption.map {
      case hh =>
        lazy val t = NumberSet.toString(hh.toInt)
        lazy val tNext = NumberSet.toString(hh.toInt+1)
        if (timeTokens.length > 1){
          val parsedNum = timeTokens.last.toInt match {
            case 0 => t + suffix
            case 15 => "viertel nach " + t + suffix
            case 30 => "halb" + NumberSet.toString(hh.toInt + 1) + suffix
            case 45 => "viertel vor " + tNext + suffix
            case mm => t + suffix + NumberSet.toString(mm)
          }

          prefix + parsedNum.replace("  "," ").trim
        }
        else prefix + t.trim + suffix
    }.getOrElse("")
  }
}

case class Am(override val t: String) extends Time {
  override def toString: String = {
    Time.ohnePrefix.contains(t) match {
      case true => t.capInitial
      case false => "am " + t.capInitial
    }
  }
}

object Time extends TokenInstance {

  val days = Set(
    "heute","morgen","gestern","wochenende","arbeittags",
    "montag","dienstag","mittwoch","donnerstag","freitag",
    "samstag","sonntag")
  val times = Set("jetzt")
  val ohnePrefix = Seq("heute","jetzt")
  val allPredef = days ++ times

  override def isInstance(token: String)(implicit rule: MasterRule) = {
    (Seq("um","am") ++ days ++ times)
      .contains(token.toLowerCase.trim)
  }
}

object Adj extends TokenInstance {
  override def isInstance(token: String)(implicit rule: MasterRule) = {
    rule.adj.contains(token.toLowerCase.trim)
  }

  def deconjugate(s: String)(implicit rule: MasterRule) = {
    rule.adj.inverseConjugateMap.getOrElse(s, s)
  }

  def empty = Adj(Nil)
}

object Adv extends TokenInstance {
  override def isInstance(token: String)(implicit rule: MasterRule) = {
    rule.adj.adv.keySet.contains(token.toLowerCase.trim)
  }
}
