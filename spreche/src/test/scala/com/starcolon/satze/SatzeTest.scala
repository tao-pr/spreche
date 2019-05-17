package com.starcolon.satze

import org.scalatest._
import Matchers._

class SatzeTest extends FunSpec with Matchers with BeforeAndAfterAll {

  implicit val rule = Rule.loadContext

  def $(str: String) = str.split(" ").toList

  describe("Nominativ"){

    it("should render nominativ sentence"){
      Satze.parse($("Er ist ein Pullover")).render() shouldBe("Er ist ein Pullover")
    }

    it("should correct gender"){
      Satze.parse($("Er ist einer Tasse")).render() shouldBe("Er ist eine Tasse")
      Satze.parse($("Er ist das Vader")).render() shouldBe("Er ist der Vader")
      Satze.parse($("Er ist der Mutter")).render() shouldBe("Er ist die Mutter")
    }

    it("should make viele plural"){
      Satze.parse($("Das ist viele Buch")).render() shouldBe("das sind viele Bucher")
    }

  }

  describe("Akkusativ"){

    it("should render akkusativ"){
      Satze.parse($("Ich habe mein kugelschreiber")).render() shouldBe(
        "Ich habe meinen Kugelschreiber")
    }

    it("should render multiple objects or subjects"){
      Satze.parse($("Der Mann und seid Freund kaufe ein Auto und ein Kugelschreiber und die Tasse")).render() shouldBe(
        "Der Mann und seid Freund kaufen ein Auto und einen Kugelschreiber und die Tasse")
    }

    it("should render some prepositions as akkusativ"){
      Satze.parse($("Wir kommen für unsere Computer")).render() shouldBe(
        "Wir kommen für unserem Computer")
    }

  }

}

