package querki.editing

import querki.test._

class EditorTests extends QuerkiTests {
  "_edit" should {
    
    "let me edit a single Property" in {
      implicit val s = commonSpace
      println(pql("""[[My Instance -> My Optional Text._edit]]"""))
    }
    
    "enable me to edit a simple Thing" in {
      // Need to have editing rights to the Space:
      implicit val user = commonSpace.owner
      
      val trivialOID = commonSpace.trivialThing.id.toString
      val trivialThingId = commonSpace.trivialThing.id.toThingId
      
      // TODO: so far, this is just a trivial sanity-check of Display Name. It should become much more robust:
      // TODO: instead of this raw string check, we should be using XhtmlParser to parse the string, and examine the
      // structure:
      processQText(commonThingAsContext(_.trivialThing), """[[_edit]]""") should
        include (s"""<input value="" type="text" class="_textEdit form-control propEditor" name="v-q-$trivialOID" data-prop=".q" data-propId="q" data-collId=".e" id="v-q-$trivialOID" data-thing="$trivialThingId" />""")      
    }
  }
}
