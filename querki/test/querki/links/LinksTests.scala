package querki.links

import querki.test._

class LinksTests extends QuerkiTests {
  // === _withParam ===
  "_withParam" should {
    "work with an external URL" in {
      class TSpace extends CommonSpace {
        val urlProp = new TestProperty(ExternalLinkType, ExactlyOne, "URL Prop")
        val myThing = new SimpleTestThing("My Thing", urlProp("http://www.google.com/"))
      }
      implicit val s = new TSpace
      
      pql("""[[My Thing -> URL Prop -> _withParam(""foo"", ""bar"")]]""") should
        equal ("[http://www.google.com/?foo=bar](http://www.google.com/?foo=bar)")
    }
  }
}