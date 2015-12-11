package querki.basic

import querki.test._

class BasicTests extends QuerkiTests {
  // === _bulleted ===
  "_bulleted" should {
    "deal correctly with nested bullet lists" in {
      class TSpace extends CommonSpace {
        val listNumsProp = new TestProperty(Core.IntType, QList, "Numbers")
        
        val myModel = new SimpleTestThing("Test Model", listNumsProp())
        val thing1 = new TestThing("Thing 1", myModel, listNumsProp(23,34,23))
        val thing2 = new TestThing("Thing 2", myModel, listNumsProp(1,2,3))
        val thing3 = new TestThing("Thing 3", myModel, listNumsProp(4,6,8))
      }
      implicit val s = new TSpace
      
      pql("""[[Test Model._instances -> ""[[Name]][[Numbers -> _bulleted]]"" -> _bulleted]]""") should
        equal("""
			<ul>
			<li class="_bullet">
			Thing 1
			<ul>
			<li class="_bullet">
			23
			</li>
			<li class="_bullet">
			34
			</li>
			<li class="_bullet">
			23
			</li>
			</ul>
			</li>
			<li class="_bullet">
			Thing 2
			<ul>
			<li class="_bullet">
			1
			</li>
			<li class="_bullet">
			2
			</li>
			<li class="_bullet">
			3
			</li>
			</ul>
			</li>
			<li class="_bullet">
			Thing 3
			<ul>
			<li class="_bullet">
			4
			</li>
			<li class="_bullet">
			6
			</li>
			<li class="_bullet">
			8
			</li>
			</ul>
			</li>
			</ul>""".strip)
    }
  }
  
  // === Computed Name ===
  "Computed Name" should {
    class TSpace extends CommonSpace {
      val linkedTo = new SimpleTestThing("Thing to Link")
      val compModel = new SimpleTestThing("Model With Computed", singleLinkProp(), Basic.ComputedNameProp("""Child of [[Single Link -> Name]]"""))
      
      val unnamed = new UnnamedThing(compModel, singleLinkProp(linkedTo))
      val named = new TestThing("My Named Thing", compModel)
    }
    
    "be used iff there isn't a Display Name" in {
      implicit val s = new TSpace

      pql("""[[Model With Computed._instances -> _commas]]""") should
        equal (s"[Child of Thing to Link](${s.unnamed.id.toThingId.toString}), [My Named Thing](My-Named-Thing)")
    }
  }
}