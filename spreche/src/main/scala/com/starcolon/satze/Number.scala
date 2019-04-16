package com.starcolon.satze

trait Number

object NumberSet {
  val map = Map(
    0   -> "null",
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
    1 -> "zehn",
    2 -> "hundert",
    3 -> "tausend"
  )

  private def translateToString(ns: Seq[Int]): String = ns match {
    case Nil      => ""
    case 0 :: nns => translateToString(nns)
    case n :: nns =>
      nns match {
        case Nil => map(n)
        case _   => 
          val remain = ns.map(_.toString).mkString("").toInt
          map.get(remain) match {
            // remaining number matches some special predefined
            case Some(s) => s
            case None => 
              val leftDigit = map(n) + power(nns.length)
              leftDigit + translateToString(nns)
          }
      }
  }

  def toString(n: Int) = {
    map.getOrElse(n, translateToString(n.toString.split("").toSeq.map(_.toInt)))
  }
}