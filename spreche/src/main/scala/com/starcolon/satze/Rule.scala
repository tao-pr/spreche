package com.starcolon.satze

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.Formats
import scala.io.Source
import com.starcolon.satze.Implicits._

import Console._

sealed trait Rule 

object NullRule extends Rule

case object PrepositionRule extends Rule {
  val mapCase = Map(
    "für" -> Akkusativ,
    "ohne" -> Akkusativ,
    "gegen" -> Akkusativ,
    "bis" -> Akkusativ,
    "durch" -> Akkusativ,
    "zu" -> Dativ,
    "in" -> Dativ,
    "ab" -> Dativ,
    "an" -> Dativ,
    "vor" -> Dativ,
    "über" -> Dativ,
    "unter" -> Dativ,
    "neben" -> Dativ,
    "auf" -> Dativ,
    "zwischen" -> Dativ,
    "hinter" -> Dativ,
    "gegenüber" -> Dativ,
    "aus" -> Dativ,
    "außer" -> Dativ,
    "nach" -> Dativ,
    "mit" -> Dativ,
    "bei" -> Dativ,
    "von" -> Dativ
  )
  def generalise(s: String) = s match {
    case "zur" | "zum" => "zu"
    case "vom" => "vor"
    case "am" => "an"
    case "ins" => "in"
    case any => any
  }
  def isPreposition(s: String) = mapCase.keySet.contains(generalise(s.toLowerCase))
}

case object AbbrevRule {
  val map = Map(
    "in dem" -> "im",
    "in das" -> "ins",
    "zu dem" -> "zum",
    "zu der" -> "zur",
    "von dem" -> "vom",
    "an dem" -> "am",
    "bei dem" -> "beim"
  )
  def foldLeft(sentence: String)(folder: (String, Tuple2[String, String]) => String) = 
    map.foldLeft(sentence)(folder)
}

case class ConjugationRule(m: Map[String, Map[String, String]]) extends Rule {

  val prefixes = Seq(
    "ein","um","fern","aus","auf","an",
    "durch","mit","hinein","rein","übrig")

  lazy val reverseMap = m.toList.flatMap{ case (v, n) => 
    n.map{ case(_,w) => (w, v) }
  }.distinct.toMap

  def isVerb(s: String) = reverseMap.keySet.contains(s)

  def isModalVerb(s: String) = ModalVerb.getList.contains(
    deconjugateVerb(s.toLowerCase)
  )
  
  def deconjugateVerb(v: String): String = {
    reverseMap.getOrElse(v, v)
  }
  
  def conjugateVerb(v: String, p: Pronoun, artikel: Artikel)(implicit rule: MasterRule) = p match {
    case _:Instance | _:PositionalPronoun => 
      val gender = rule.sache.findGender(p.s.capInitial)
      val genderedPronoun = (artikel, gender) match {
        case (Plural,_) => Wir
        case (_,"der") => Es
        case (_,"die") => Sie
        case (_,"das") => Es
        case _ => Es
      }
      m.getOrElse(v, Map(genderedPronoun.s -> v)).getOrElse(genderedPronoun.s, v)
    case _ => m.getOrElse(v, Map(p.s -> v)).getOrElse(p.s, v)
  }

  def conjugatePerfektVerb(v: String, p: Pronoun)(implicit rule: MasterRule) = {
    val a = Ein
    m.getOrElse(v, Map("perfekt" -> v)).getOrElse("perfekt", v).split(" ").toSeq match {
      case Seq("habe", w: String) => (conjugateVerb("haben",p,a), w)
      case Seq("bin", w: String) => (conjugateVerb("seid",p,a), w)
    }
  }

  def separate(v: String): Option[(String, String)] = {
    prefixes.find(v.startsWith(_)).map{ pref =>
      (pref, v.drop(pref.length))
    }
  }

  def isSeparable(v: String) = separate(v).isDefined

  override def toString = m.map{ case(_, n) => 
    n.map{ case(p, v) => s"${p} ${v}"}.mkString(" | ") 
  }.mkString("\n")
}

sealed case class Sache(s: String, gender: String, plural: String) extends Rule {
  override def toString = s"${gender} $s"
}

case class SacheRule(m: Map[String, Sache]) extends Rule {
  lazy val all = m.keys.toSet ++ m.map{ case(_, s) => s.plural }.toSet
  def isSache(s: String) = all.contains(s)
  def findGender(s: String): String = m.get(s).map(_.gender).getOrElse("")
  def findPlural(s: String): String = m.get(s).map(_.plural).getOrElse(s)
  override def toString = m.map{ case(_, sache) => sache.toString }.mkString("\n")
}

sealed case class AdjSubRule(mean: String) extends Rule
sealed case class AdvSubRule(mean: String) extends Rule

case class AdjRule(
  adj: Map[String, AdjSubRule], 
  adv: Map[String, AdvSubRule]) 
extends Rule {

  lazy val adjStemmed = adj.keySet.flatMap{ j => 
    Set(j, j.ensureEnding("en"), j.ensureEnding("es"), j.ensureEnding("e"))
  }.reduce(_ ++ _)

  lazy val inverseConjugateMap = (adj.keySet ++ adv.keySet).flatMap{ j => 
    Set(j, j.ensureEnding("en"), j.ensureEnding("es"), j.ensureEnding("e")).map{
      j_ => (j_, j)
    }.toList
  }.toMap

  override def toString = s"Adj : ${adj.keySet}\nAdv: ${adv.keySet}"

  def contains(s: String) = adjStemmed.contains(s)
}

case class MasterRule(
  conjugation: ConjugationRule, 
  sache: SacheRule,
  adj: AdjRule) 
extends Rule 

object Rule {

  private def fromFile(fname: String): String = Source
    .fromInputStream(getClass.getResourceAsStream("/" + fname))
    .getLines().mkString("\n")

  def loadConjugationRule: ConjugationRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    ConjugationRule(parse(fromFile("conjugation.json")).extract[Map[String, Map[String, String]]])
  }

  def loadSacheRule: SacheRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    SacheRule(parse(fromFile("sache.json")).extract[Map[String, List[String]]].map{
      case (sache, ns) => (sache, Sache(sache, ns.head, ns.last))
    })
  }

  def loadAdjRule: AdjRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    val raw = parse(fromFile("adj.json")).extract[Map[String, Map[String, Map[String, String]]]]

    val adj = raw("adj").map{ case (w,attr) => (w, AdjSubRule(attr.getOrElse("mean", "")))}
    val adv = raw("adv").map{ case (w,attr) => (w, AdvSubRule(attr.getOrElse("mean", "")))}

    AdjRule(adj, adv)
  }

  def loadContext: MasterRule = {
    implicit val formats: Formats = DefaultFormats.withStrictOptionParsing.withStrictArrayExtraction
    val conjugation = loadConjugationRule
    val sache = loadSacheRule
    val adj = loadAdjRule
    
    println(GREEN)
    println("Rules loaded")
    println(RESET)

    println(adj)
    
    MasterRule(conjugation, sache, adj)
  }
}