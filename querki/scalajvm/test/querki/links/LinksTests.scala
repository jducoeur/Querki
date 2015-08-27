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
      
      pql("""[[My Thing -> URL Prop -> _withParam(""foo"", ""bar"", true)]]""") should
        equal ("[http://www.google.com/](http://www.google.com/?foo=bar)")
    }
    
    "work with a Link" in {
      implicit val s = commonSpace
      
      pql("""[[Sandbox -> _withParam(""foo"", ""bar"")]]""") should
        equal ("""[Sandbox](Sandbox?foo=%22%22bar%22%22)""")
    }
    
    "work with a Link and a Link parameter" in {
      implicit val s = commonSpace
      
      pql("""[[Sandbox -> _withParam(""foo"", My Instance)]]""") should
        equal (s"[Sandbox](Sandbox?foo=${s.instance.id.toThingId})")
    }
  }
  
  // === QURL ===
  "QURL" should {
    // This example taken from a URL I found in the wild, which was failing:
    "cope with a messy URL" in {
      QURL("http://gallica.bnf.fr/ark:/12148/btv1b8451602n/f241.image.r=Latin%2010286.langEN")
    }
  }
  
  "_oidLink" should {
    "work" in {
      implicit val s = commonSpace
      
      pql("""[[My Instance -> _oidLink]]""") should
        equal(s"[My Instance](${s.instance.id.toThingId})")
    }
  }
}
