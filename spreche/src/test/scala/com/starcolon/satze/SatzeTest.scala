package com.starcolon.satze

import org.scalatest._
import Matchers._

import com.starcolon.satze._
import com.starcolon.satze.Implicits._

class SatzeTest extends FunSpec with Matchers with BeforeAndAfterAll {

  implicit val rule = Rule.loadContext

  def $(str: String): String = Satze.parse(str.split(" ").toList).render()

  describe("Utils / Commons"){

    val NoAdj = Adj(Nil)
    val satze = Satze(
        TimeClaus(None, Some(Um("sieben"))) ::
        VerbClaus(Verb("gehen")) ::
        SubjectClaus((NoArtikel, NoAdj, Wir) :: Nil) ::
        ObjectClaus(Some(Preposition("zu")), ((Der, NoAdj, Instance("Kino"))) :: Nil) ::
        Nil
      )

    it("should identify order of clauses"){

      satze.isAfter[TimeClaus, VerbClaus] shouldBe true
      satze.isAfter[VerbClaus, ObjectClaus] shouldBe true
      satze.isAfter[ObjectClaus, SubjectClaus] shouldBe false
      satze.isAfter[VerbClaus, ModalVerbClaus] shouldBe false
    }

  }

  describe("Nominativ"){

    it("should render nominativ sentence"){
      $("Er ist ein Pullover") shouldBe(
        "er ist ein Pullover")
    }

    it("should correct gender"){
      $("Er ist einer Tasse") shouldBe(
        "er ist eine Tasse")
      $("Er ist das Vader") shouldBe(
        "er ist der Vader")
      $("Er ist der Mutter") shouldBe(
        "er ist die Mutter")
    }

    it("should make viele plural"){
      $("Das ist viele Buch") shouldBe(
        "das sind viele Bucher")
    }

    it("should render nicht"){
      $("Das sind nicht mein auto") shouldBe(
        "das ist mein Auto nicht")
    }

  }

  describe("Akkusativ"){

    it("should render akkusativ"){
      $("Ich habe mein kugelschreiber") shouldBe(
        "ich habe meinen Kugelschreiber")
    }

    it("should render multiple objects or subjects"){
      $("Der Mann und sein Freund kaufe ein Auto und ein Kugelschreiber und die Tasse") shouldBe(
        "der Mann und sein Freund kaufen ein Auto und einen Kugelschreiber und die Tasse")
    }

    it("should render some prepositions as akkusativ"){
      $("Wir kommen für unsere Computer") shouldBe(
        "wir kommen für unseren Computer")
      $("Er laufen durch der park oder der kirche oder ein büro") shouldBe(
        "er läuft durch den Park oder die Kirche oder ein Büro")
    }

    it("should render nicht"){
      $("Mein Freund und ihr Freund kenne mich nicht") shouldBe(
        "mein Freund und ihr Freund kennen mich nicht")
    }

  }

  describe("Dativ"){

    it("should render simple Dativ"){
      $("Wir gehen in das Kino") shouldBe(
        "wir gehen im Kino"
      )
      $("Er laufe zu das haus") shouldBe(
        "er läuft zum Haus"
      )
      $("sie gehe zu dir") shouldBe(
        "sie geht zu dir"
      )
      $("Er laufe zu die schule") shouldBe(
        "er läuft zur Schule"
      )
      $("wir fragen mit die frau von die ausland") shouldBe(
        "wir fragen mit der Frau vom Ausland"
      )
    }

    it("should render two objects as dativ"){
      $("Mein Frau kauft mich eine Kaffee") shouldBe(
        "meine Frau kauft mir einen Kaffee"
      )
      $("Wir bringt uns er") shouldBe(
        "wir bringen uns ihn"
      )
      $("Wir bringt er sie") shouldBe(
        "wir bringen ihm sie"
      )
      $("ein Mann gibt der nachbar der keks") shouldBe(
        "ein Mann gibt dem Nachbar den Keks"
      )
    }

    it("should abbreviate zu der to zur, and so on"){
      $("wir gehen zu der arzt in der stadt") shouldBe(
        "wir gehen zum Arzt im Stadt")
      $("er laufe von der vader zu der nachbar") shouldBe(
        "er läuft vom Vader zum Nachbar")
      $("du stellen ein glas von der schrank an den tisch") shouldBe(
        "du stellst ein Glas vom Schrank am Tisch")
      $("ich bin zu der stadt") shouldBe(
        "ich bin zum Stadt")
      $("ich bin zu der schule") shouldBe(
        "ich bin zur Schule")
      $("wir gehen in das kino") shouldBe(
        "wir gehen im Kino")
    }
  }

  describe("Modal verbs"){

    it("should render modal verb"){
      $("Ich soll treffe mit mein Freund") shouldBe(
        "ich soll mit meinem Freund treffen")
    }

    it("should render modal verb with dativ"){
      $("Ich soll bringe du meinen kugelschreiber") shouldBe(
        "ich soll dir meinen Kugelschreiber bringen")
    }
  }

  describe("Time"){
 
    it("should render halb"){
      $("Ich um 17:30 uhr kommen in das kino") shouldBe(
        "um halbachtzehn Uhr komme ich im Kino")
    }

    it("should render viertel"){
      $("Wir um 17:45 soll kommen in das kino") shouldBe(
        "um viertel vor achtzehn Uhr sollen wir im Kino kommen") 
    }

    it("should render day"){
      $("Ich am samstag will gehen mit mein Freund") shouldBe(
        "am Samstag will ich mit meinem Freund gehen") 
    }

    it("should render time & day"){
      $("Ich heute um 6:20 putzt mein tisch") shouldBe(
        "Heute um sechs Uhr zwanzig putze ich meinen Tisch") 
    }

    it("should render nicht"){
      $("Ich treffe mit mein kollege am arbeittags nicht") shouldBe(
        "am Arbeittags treffe ich mit meinem Kollege nicht")
    }
  }

  describe("Separable verbs"){

    it("should separate verbs in akkusativ"){
      $("wir einsteigen die tram") shouldBe(
        "wir steigen die Tram ein")
      $("ich aussteigen das auto") shouldBe(
        "ich steige das Auto aus")
      $("er einsteigen das auto") shouldBe(
        "er steigt das Auto ein")
    }

    it("should separate verbs in mixture of dativ and akkusativ"){
      $("mein freund und ich einsteigen die tram zu unser büro") shouldBe(
        "mein Freund und ich steigen die Tram zu unserem Büro ein")
    }

    it("should not separate verbs when verb is placed at the end (with modalverb)"){
      $("sie soll anhalten mit ihre mutter") shouldBe(
        "sie soll mit ihre Mutter anhalten")
      $("sie anhalten mit ihre mutter") shouldBe(
        "sie hält mit ihre Mutter an")
    }

    it("should not separate verbs when time claus is present"){
      $("sie soll heute anhalten mit ihre mutter") shouldBe(
        "Heute soll sie mit ihre Mutter anhalten")
      $("sie um 7:30 anhalten mit ihre mutter") shouldBe(
        "um halbacht Uhr hält sie mit ihre Mutter an")
    }
  }

  describe("Adjective conjugation"){
    
    it("should conjugate nominativ adjectives"){
      $("das ist ein nett mann") shouldBe(
        "das ist ein netter Mann")
      $("das ist der nett mann") shouldBe(
        "das ist der nette Mann")
      $("das ist ein groß tasse") shouldBe(
        "das ist eine große Tasse")
      $("es ist ein groß kino") shouldBe(
        "es ist ein großes Kino")
    }

    it("should conjugate dativ adjectives"){
      $("sie sitzen neben der groß Park") shouldBe(
        "sie sitzt neben dem großen Park")
      $("ihr kauft ein alt mann ein taschentuch") shouldBe(
        "ihr kauft einem alten Mann ein Taschentuch")
      $("wir gehen zu der neu Kino") shouldBe(
        "wir gehen zum neuen Kino")
    }

    it("should conjugate akkusativ adjectives"){
      $("wir kaufe ein klein tisch") shouldBe(
        "wir kaufen einen kleinen Tisch")
      $("wir kaufe ein orange lampe") shouldBe(
        "wir kaufen eine orange Lampe")
      $("wir kaufe die klein lampe") shouldBe(
        "wir kaufen die kleine Lampe")
      $("wir kaufe ein rot glas") shouldBe(
        "wir kaufen ein rotes Glas")
      $("wir kaufe das rot glas") shouldBe(
        "wir kaufen das rote Glas")
    }

    it("should not conjugate adjectives when the object is not present"){
      $("wir sind alt") shouldBe(
        "wir sind alt")
      $("unser haus ist rot") shouldBe(
        "unser Haus ist rot")
      $("dein kugelschreiber ist dünnes") shouldBe(
        "dein Kugelschreiber ist dünn")
    }

  }

  describe("Adverb"){

    it("should add adverb to sentence"){
      $("mein auto ist sehr schön") shouldBe(
        "mein Auto ist sehr schön")
      $("unser auto liege dabei neben den park") shouldBe(
        "unser Auto liegt dabei neben dem Park")
    }

    it("should add adverbs to complicated sentence"){
      $("um 8 uhr am montag wir leider haben keinen neuen termin mit unser kunde") shouldBe(
        "am Montag um acht Uhr leider haben wir keinen neuen Termin mit unserem Kunde")
    }

  }

  describe("Perfekt tense"){

    it("should conjugate verbs into perfekt tense"){
      $("wir haben eine tasse kaufen") shouldBe(
        "wir haben eine Tasse gekauft")
      $("sie hat ein frühstück machen") shouldBe(
        "sie hat ein Frühstück gemacht")
      $("sie hat ein frühstück machen nicht") shouldBe(
        "sie hat ein Frühstück nicht gemacht")
      $("wir haben weiterempfehlen ihn ein neu buch") shouldBe(
        "wir haben ihm ein neues Buch weiterempfohlen")
    }

    it("should reform perfekt verbs into correct position"){
      $("ich bin neben sein stadt gehen nicht") shouldBe(
        "ich bin neben seinem Stadt nicht gegangen")
      $("ich habe nicht gehen neben sein stadt") shouldBe(
        "ich bin neben seinem Stadt nicht gegangen")
    }

    it("should render with time claus"){
      $("dein vader und du um 7 hat zu mir gehen") shouldBe(
        "um sieben Uhr sind dein Vader und du zu mir gegangen")
      $("wir heute haben weiterempfehlen ihn ein neu buch") shouldBe(
        "Heute haben wir ihm ein neues Buch weiterempfohlen")
    }

  }

  describe("Diese conjugation"){
    it("should conjugate diese by nominativ"){
      $("diese mann kennst du nicht") shouldBe(
        "dieser Mann kennt dich nicht")
      $("dieser frau soll zu der schule gehen") shouldBe(
        "diese Frau soll zur Schule gehen")
      $("diese glas ist sehr schön") shouldBe(
        "dieses Glas ist sehr schön")
    }

    it("should conjugate diese by akkusativ"){
      $("diese mann braucht diese orangensaft") shouldBe(
        "dieser Mann braucht diesen Orangensaft")
      $("diese bahn fährt durch diese Schule") shouldBe(
        "diese Bahn fährt durch diese Schule")
      $("diese Papier liegt in diese Zimmer") shouldBe(
        "dieses Papier liegt in diesem Zimmer")
    }
  }

  describe("Reflective verbs"){
    
  }

  describe("Hauptsatze und Nebensatze"){

  }

}

