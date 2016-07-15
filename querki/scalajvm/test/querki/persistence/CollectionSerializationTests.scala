package querki.persistence

import models._

import querki.globals._
import querki.test._

class CollectionSerializationTests extends QuerkiTests {
  
  class CollSpace extends CommonSpace {
    val linkableThing = new SimpleTestThing("Linkable Thing")
    val linkable2 = new SimpleTestThing("Linkable 2")
    
    val oneThing = new SimpleTestThing("One Thing", singleTextProp("hello"), singleTagProp("floob"), singleLinkProp(linkableThing))
    val optThing = new SimpleTestThing("Opt Thing", optTextProp("hello"), optTagProp("floob"), optLinkProp(linkableThing))
    val emptyOptThing = new SimpleTestThing("Empty Opt Thing", optTextProp(), optTagProp(), optLinkProp())
    val listThing = new SimpleTestThing("List Thing",
      listTagsProp("foo", "bar"),
      listLinksProp(linkable2, linkableThing)
    )
    val emptyListThing = new SimpleTestThing("Empty List Thing",
      listTagsProp(),
      listLinksProp()
    )
  }
  
  def roundtripProp(t:Thing, prop:AnyProp)(implicit s:CollSpace) = {
    implicit val space = s.state
    val v = t.getPropVal(prop)
    val ser = prop.serialize(v)
    val copy = prop.deserialize(ser)
    assert(copy.matches(v))
  }
  
  def checkProp(t:Thing, prop:AnyProp, ser:String)(implicit s:CollSpace) = {
    implicit val space = s.state
    val v = t.getPropVal(prop)
    val copy = prop.deserialize(ser)
    assert(copy.matches(v))
  }
  
  "ExactlyOne" should {
    "roundtrip for several ptypes" in {
      implicit val s = new CollSpace
      
      val t = s.oneThing
      roundtripProp(t, s.singleTextProp)
      roundtripProp(t, s.singleTagProp)
      roundtripProp(t, s.singleLinkProp)
    }
    
    "deserialize from a raw string" in {
      implicit val s = new CollSpace
      
      // The old-style version of ExactlyOne serialization:
      checkProp(s.oneThing, s.singleTagProp, "floob")
    }
  }
  
  "Optional" should {
    "roundtrip for several ptypes" in {
      implicit val s = new CollSpace
      
      val t = s.optThing
      roundtripProp(t, s.optTextProp)
      roundtripProp(t, s.optTagProp)
      roundtripProp(t, s.optLinkProp)
    }    
    
    "roundtrip for several empty values" in {
      implicit val s = new CollSpace
      
      val t = s.emptyOptThing
      roundtripProp(t, s.optTextProp)
      roundtripProp(t, s.optTagProp)
      roundtripProp(t, s.optLinkProp)
    }
    
    "deserialize from a raw string" in {
      implicit val s = new CollSpace
      
      // The old-style version of Optional serialization:
      checkProp(s.optThing, s.optTagProp, "(floob)")
      checkProp(s.emptyOptThing, s.optTagProp, "!")
    }
  }
  
  "QList" should {
    "roundtrip for several ptypes" in {
      implicit val s = new CollSpace
      
      val t = s.listThing
      roundtripProp(t, s.listTagsProp)
      roundtripProp(t, s.listLinksProp)
    }    
    
    "roundtrip for several empty values" in {
      implicit val s = new CollSpace
      
      val t = s.emptyListThing
      roundtripProp(t, s.listTagsProp)
      roundtripProp(t, s.listLinksProp)
    }
    
    "deserialize from a raw string" in {
      implicit val s = new CollSpace
      
      // List-style serialization:
      checkProp(s.listThing, s.listTagsProp, "[foo,bar]")
      checkProp(s.emptyListThing, s.listTagsProp, "[]")
    }
  }
}
