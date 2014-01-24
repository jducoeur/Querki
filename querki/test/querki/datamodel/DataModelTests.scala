package querki.datamodel

import querki.test._

class DataModelTests extends QuerkiTests {
  "_hasProperty" should {
    "produce true iff the Thing has the Property" in {
      processQText(commonThingAsContext(_.instance), """[[_hasProperty(My Optional Text._self)]]""") should 
        equal ("""true""")      
    }
    
    "produce false iff the Thing doesn't have the Property" in {
      processQText(commonThingAsContext(_.withDisplayName), """[[_hasProperty(My Optional Text._self)]]""") should 
        equal ("""false""")      
    }
    
    "error if there are no parameters" in {
      processQText(commonThingAsContext(_.withDisplayName), """[[_hasProperty]]""") should 
        equal (expectedWarning("Func.missingParam"))      
    }
    
    "error if it receives a non-Thing" in {
      processQText(commonThingAsContext(_.instance), """[[My Optional Text -> _hasProperty(My Optional Text._self)]]""") should 
        equal (expectedWarning("Func.notThing"))      
    }
    
    "error if the parameter isn't a Thing" in {
      processQText(commonThingAsContext(_.instance), """[[_hasProperty(My Optional Text)]]""") should 
        equal (expectedWarning("Func.paramNotThing"))      
    }
  }
}