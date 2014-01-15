package querki.editing

import querki.test._

class EditorTests extends QuerkiTests {
  "_edit" should {
    "enable me to edit a simple Thing" in {
      // Need to have editing rights to the Space:
      implicit val user = commonSpace.owner
      
      val trivialOID = commonSpace.trivialThing.id.toString
      val trivialThingId = commonSpace.trivialThing.id.toThingId
      
      // TODO: so far, this is just a trivial sanity-check of Display Name. It should become much more robust:
      processQText(commonThingAsContext(_.trivialThing), """[[_edit]]""") should 
        include (s"""<input data-thing='$trivialThingId' id='v-q-$trivialOID' data-propId='q' data-prop='.q' name='v-q-$trivialOID' class='propEditor' type='text' value=''/>""")      
    }
  }
}
