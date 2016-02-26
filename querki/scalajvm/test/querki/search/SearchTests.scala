package querki.search

import models.UnknownOID

import querki.globals._
import querki.test._

/**
 * @author jducoeur
 */
class SearchTests extends QuerkiTests {
  lazy val Search = interface[Search]
  
  implicit class resultChecker(resOpt:Option[SearchResultsInternal]) {
    def shouldHave(f:SearchResultInternal => Boolean) = {
      resOpt match {
        case Some(res) => {
          val answer = res.results.find(f)
          assert(answer.isDefined, s"Didn't get expected result from search(${res.request})!")
        }
        case _ => fail("Got None from search()!")
      }
    }
    
    def shouldntHave(f:SearchResultInternal => Boolean) = {
      resOpt match {
        case Some(res) => {
          val answer = res.results.find(f)
          assert(answer.isEmpty, s"Found unexpected search result $answer from search(${res.request})!")
        }
        case None => // That's fine in this case
      }
    }
  }
  
  "search()" should {
    "find display names" in {
      class TSpace extends CommonSpace {
        val bluebox = new SimpleTestThing("b2", Basic.DisplayNameProp("Blue Box"))
        val cello = new SimpleTestThing("b3", Basic.DisplayNameProp("Cello"))
      }
      implicit val s = new TSpace
      
      val results = Search.search("box")(s.state)
      results.shouldHave(_.thing == s.sandbox)
      results.shouldHave(res => res.thing == s.bluebox && res.positions.contains(5))
      results.shouldntHave(_.thing == s.cello)
    }
    
    "find text values" in {
      class TSpace extends CommonSpace {
        val t1 = new SimpleTestThing("t1", optTextProp("Now is the winter of our discontent"))
        val t2 = new SimpleTestThing("t2", optTextProp("made glorious summer"))
      }
      implicit val s = new TSpace
      
      val results = Search.search("glorious")(s.state)
      results.shouldHave(res => res.thing == s.t2 && res.positions.contains(5))
      results.shouldntHave(_.thing == s.t1)
    }
    
    "find tags" in {
      class TSpace extends CommonSpace {
        val tagThing = new SimpleTestThing("tagThing", listTagsProp("This is a tag", "another tag", "But this is not"))
      }
      implicit val s = new TSpace
      
      val results = Search.search("tag")(s.state)
      results.shouldHave(res => res.thing.id == UnknownOID && res.text == "another tag" && res.positions.contains(8))
      results.shouldntHave(res => res.text == "But this is not")
    }
  }
  
  "_search" should {
    class TSpace extends CommonSpace {
      val model1 = new SimpleTestThing("Model 1")
      val model2 = new SimpleTestThing("Model 2")
      
      val myTagProp = new TestProperty(TagType, ExactlyOne, "Modeled Tag", Links.LinkModelProp(model2))
      
      val thing1 = new TestThing("Thing 1", model1, singleTextProp("Some random text"), optTextProp("Something else"))
      val thing2 = new TestThing("Thing 2", model1, singleTextProp("Blurdy-blurdy-blur"))
      val thing3 = new TestThing("Thing 3", model2, singleTextProp("Also random!"))
      val thing6 = new TestThing("Thing 6", model1, optTextProp("My randomness knows no bounds"))
      
      val thing4 = new SimpleTestThing("Thing 4", myTagProp("And I too am random"))
      val thing5 = new SimpleTestThing("Thing 5", singleTagProp("Abounding in randomness"))
    }
    
    "use the models parameter" in {
      implicit val s = new TSpace
      
      pql("""[[_search(query=""random"", searchTags=false, models=Model 2) -> _searchResultThing -> Link Name]]""") should
        equal("Thing 3")
      pql("""[[_search(query=""random"", searchThings=false, models=Model 2) -> _searchResultTag]]""") should
        equal(oneTag("And I too am random"))
    }
    
    "use the properties parameter" in {
      implicit val s = new TSpace
      
      pql("""[[_search(query=""random"", searchTags=false, properties=My Optional Text._self) -> _searchResultThing -> Link Name]]""") should
        equal("Thing 6")      
    }
  }
}
