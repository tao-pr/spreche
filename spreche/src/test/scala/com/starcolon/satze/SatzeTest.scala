package com.starcolon.satze

import org.scalatest._
import Matchers._

import com.starcolon.satze._
import com.starcolon.satze.Implicits._

class SatzeTest extends FunSpec with Matchers with BeforeAndAfterAll {

  implicit val rule = Rule.loadContext

  def $(str: String): List[String] = str.split(" ").toList

  describe("Nominativ"){

    it("should render nominativ sentence"){
      Satze.parse($("Er ist ein Pullover")).render() shouldBe(
        "er ist ein Pullover")
    }

    it("should correct gender"){
      Satze.parse($("Er ist einer Tasse")).render() shouldBe(
        "er ist eine Tasse")
      Satze.parse($("Er ist das Vader")).render() shouldBe(
        "er ist der Vader")
      Satze.parse($("Er ist der Mutter")).render() shouldBe(
        "er ist die Mutter")
    }

    it("should make viele plural"){
      Satze.parse($("Das ist viele Buch")).render() shouldBe(
        "das sind viele Bucher")
    }

    it("should render nicht"){
      Satze.parse($("Das sind nicht mein auto")).render() shouldBe(
        "das ist mein Auto nicht")
    }

    it("should render adj"){
      Satze.parse($("Mein haus ist sehr alt")).render() shouldBe(
        "mein Haus ist sehr alt")
    }

    it("should render multiple adjs"){
      Satze.parse($("Wir ist zu ungewiss und fleißig")).render() shouldBe(
        "wir sind zu ungewiss fleißig")
    }
  }

  describe("Akkusativ"){

    it("should render akkusativ"){
      Satze.parse($("Ich habe mein kugelschreiber")).render() shouldBe(
        "ich habe meinen Kugelschreiber")
    }

    it("should render multiple objects or subjects"){
      Satze.parse($("Der Mann und sein Freund kaufe ein Auto und ein Kugelschreiber und die Tasse")).render() shouldBe(
        "der Mann und sein Freund kaufen ein Auto und einen Kugelschreiber und die Tasse")
    }

    it("should render some prepositions as akkusativ"){
      Satze.parse($("Wir kommen für unsere Computer")).render() shouldBe(
        "wir kommen für unseren Computer")
    }

    it("should render some prepositions as akkusativ (2)"){
      Satze.parse($("Er laufen durch der park oder der kirche oder ein büro")).render() shouldBe(
        "er läuft durch den Park oder die Kirche oder ein Büro")
    }

    it("should render nicht"){
      Satze.parse($("Mein Freund und ihr Freund kenne mich nicht")).render() shouldBe(
        "mein Freund und ihr Freund kennen mich nicht")
    }

  }

  describe("Dativ"){

    it("should render simple Dativ"){}

    it("should render some prepositions as dativ"){}

    it("should render two objects as dativ"){}

    it("should mix akkusativ with dativ"){}
  }

  describe("Modal verbs"){

  }

  describe("Time"){
 
    it("should render halb"){
      Satze.parse($("Ich um 17:30 uhr kommen in das kino")).render() shouldBe(
        "um halbachtzehn Uhr komme ich ins Kino")
    }

    it("should render viertel"){
      Satze.parse($("Wir um 17:45 soll kommen in das kino")).render() shouldBe(
        "um viertel vor achtzehn Uhr sollen wir ins Kino kommen") 
    }

    it("should render day"){
      Satze.parse($("Ich am samstag will gehen mit mein Freund")).render() shouldBe(
        "am Samstag will ich mit meinen Freund gehen") 
    }

    it("should render time & day"){
      Satze.parse($("Ich heute um 6:20 putzt mein tisch")).render() shouldBe(
        "Heute um sechs Uhr zwanzig putzen ich meinen Tisch") 
    }
  }

}

