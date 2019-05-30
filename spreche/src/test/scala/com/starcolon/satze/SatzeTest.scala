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
      Satze.parse($("Er laufen durch der park oder der kirche oder ein büro")).render() shouldBe(
        "er läuft durch den Park oder die Kirche oder ein Büro")
    }

    it("should render nicht"){
      Satze.parse($("Mein Freund und ihr Freund kenne mich nicht")).render() shouldBe(
        "mein Freund und ihr Freund kennen mich nicht")
    }

  }

  describe("Dativ"){

    it("should render simple Dativ"){
      Satze.parse($("Wir gehen in das Kino")).render() shouldBe(
        "wir gehen im Kino"
      )
      Satze.parse($("Er laufe zu das haus")).render() shouldBe(
        "er läuft zum Haus"
      )
      Satze.parse($("sie gehe zu dir")).render() shouldBe(
        "sie geht zu dir"
      )
      Satze.parse($("Er laufe zu die schule")).render() shouldBe(
        "er läuft zur Schule"
      )
      Satze.parse($("wir fragen mit die frau von die ausland")).render() shouldBe(
        "wir fragen mit der Frau vom Ausland"
      )
    }

    it("should render two objects as dativ"){
      Satze.parse($("Mein Frau kauft mich eine Kaffee")).render() shouldBe(
        "meine Frau kauft mir einen Kaffee"
      )
      Satze.parse($("Wir bringt uns er")).render() shouldBe(
        "wir bringen uns ihn"
      )
      Satze.parse($("Wir bringt er sie")).render() shouldBe(
        "wir bringen ihm sie"
      )
      Satze.parse($("ein Mann gibt der nachbar der keks")).render() shouldBe(
        "ein Mann gibt dem Nachbar den Keks"
      )
    }

    it("should abbreviate zu der to zur, and so on"){
      Satze.parse($("wir gehen zu der arzt in der stadt")).render() shouldBe(
        "wir gehen zum Arzt im Stadt")
      Satze.parse($("er laufe von der vader zu der nachbar")).render() shouldBe(
        "er läuft vom Vader zum Nachbar")
      Satze.parse($("du stellen ein glas von der schrank an den tisch")).render() shouldBe(
        "du stellst ein Glas vom Schrank am Tisch")
      Satze.parse($("ich bin zu der stadt")).render() shouldBe(
        "ich bin zum Stadt")
      Satze.parse($("ich bin zu der schule")).render() shouldBe(
        "ich bin zur Schule")
      Satze.parse($("wir gehen in das kino")).render() shouldBe(
        "wir gehen im Kino")
    }
  }

  describe("Modal verbs"){

    it("should render modal verb"){
      Satze.parse($("Ich soll treffe mit mein Freund")).render() shouldBe(
        "ich soll mit meinem Freund treffen")
    }

    it("should render modal verb with dativ"){
      Satze.parse($("Ich soll bringe du meinen kugelschreiber")).render() shouldBe(
        "ich soll dir meinen Kugelschreiber bringen")
    }
  }

  describe("Time"){
 
    it("should render halb"){
      Satze.parse($("Ich um 17:30 uhr kommen in das kino")).render() shouldBe(
        "um halbachtzehn Uhr komme ich im Kino")
    }

    it("should render viertel"){
      Satze.parse($("Wir um 17:45 soll kommen in das kino")).render() shouldBe(
        "um viertel vor achtzehn Uhr sollen wir im Kino kommen") 
    }

    it("should render day"){
      Satze.parse($("Ich am samstag will gehen mit mein Freund")).render() shouldBe(
        "am Samstag will ich mit meinem Freund gehen") 
    }

    it("should render time & day"){
      Satze.parse($("Ich heute um 6:20 putzt mein tisch")).render() shouldBe(
        "Heute um sechs Uhr zwanzig putze ich meinen Tisch") 
    }

    it("should render nicht"){
      Satze.parse($("Ich treffe mit mein kollege am arbeittags nicht")).render() shouldBe(
        "am Arbeittags treffe ich mit meinem Kollege nicht")
    }
  }

  describe("Separable verbs"){

    it("should separate verbs in akkusativ"){
      Satze.parse($("wir einsteigen die tram")).render() shouldBe(
        "wir steigen die Tram ein")
      Satze.parse($("ich aussteigen das auto")).render() shouldBe(
        "ich steige das Auto aus")
      Satze.parse($("er einsteigen das auto")).render() shouldBe(
        "er steigt das Auto ein")
    }

    it("should separate verbs in mixture of dativ and akkusativ"){
      Satze.parse($("mein freund und ich einsteigen die tram zu unser büro")).render() shouldBe(
        "mein Freund und ich steigen die Tram zu unserem Büro ein")
    }

    it("should not separate verbs when verb is placed at the end (with modalverb)"){
      Satze.parse($("sie soll anhalten mit ihre mutter")).render() shouldBe(
        "sie soll mit ihre Mutter anhalten")
      Satze.parse($("sie anhalten mit ihre mutter")).render() shouldBe(
        "sie hält mit ihre Mutter an")
    }

    it("should not separate verbs when time claus is present"){
      Satze.parse($("sie soll heute anhalten mit ihre mutter")).render() shouldBe(
        "Heute soll sie mit ihre Mutter anhalten")
      Satze.parse($("sie um 7:30 anhalten mit ihre mutter")).render() shouldBe(
        "um halbacht Uhr hält sie mit ihre Mutter an")
    }
  }

  describe("Perfekt tense"){

  }

  describe("Hauptsatze und Nebensatze"){

  }

}

