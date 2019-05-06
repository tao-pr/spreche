package com.starcolon.satze

trait Number

object NumberSet {
  val map = Map(
    0   -> "",
    1   -> "eins",
    2   -> "zwei",
    3   -> "drei",
    4   -> "vier",
    5   -> "fünf",
    6   -> "sechs",
    7   -> "sieben",
    8   -> "acht",
    9   -> "neun",
    10  -> "zehn",
    11  -> "elf",
    12  -> "zwölf",
    20  -> "zwanzig",
    100 -> "hundert",
    1000 -> "tausend"
  )

  val power = Map(
    0 -> "",
    1 -> "zig",
    2 -> "hundert",
    3 -> "tausend"
  )

  private def abbrevNumber(sNum: String) = sNum match {
    case "eins" => "ein"
    case "seiben" => "sieb"
    case _ => sNum
  }

  private def translateToString(ns: Seq[Int]): String = {
    lazy val nsAsNum = ns.map(_.toString).mkString("").toInt
    ns match {
      case Nil      => ""
      case _ if map.keySet.contains(nsAsNum) => map(nsAsNum)
      case 0 :: nns => translateToString(nns)
      case a :: b :: Nil => 
        val right = b match {
          case 0 => ""
          case _ => map(b) + "und"
        }
        a match {
          case 1 => map(b) + "zehn"
          case 2 => right + "zwanzig"
          case _ => right + abbrevNumber(map(a)) + "zig"
        }
      case n :: nns =>
        abbrevNumber(map(n)) + power(nns.length) + translateToString(nns)
    }
  }

  def toString(n: Int): String = {
    map.getOrElse(n, translateToString(n.toString.split("").map(_.toInt).toList))
  }
}