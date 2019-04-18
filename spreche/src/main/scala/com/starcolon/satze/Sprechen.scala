package com.starcolon.satze

import Console._
import scala.util.{Try, Success}

import com.starcolon.satze._
import com.starcolon.satze.Implicits._

object Sprechen {
  val voiceOn = false

  def conversation(implicit rule: MasterRule): Unit = {
    readLine("Sagen Sie > ").split(" ").filter(_.trim.size > 0).toList match {
      // Single numeric input token
      case n :: Nil => 
        Try { n.toInt } match {
          case Success(num) => 
            val parsedNum = NumberSet.toString(num)
            println(s"${MAGENTA}${parsedNum}${RESET}")
            
            if (voiceOn)
              parsedNum.speak
            
            conversation

          case _ => println(YELLOW + s"Unsupported token : $n" + RESET)
        }
      
      case Nil => conversation

      // Input tokens
      case inputTokens =>
        val inputSatze = Satze.parse(inputTokens)
        println(inputSatze)
        val out = inputSatze.render()
        println(s"${MAGENTA}${out}${RESET}")
        out.speak
        conversation
    } 
  }
}