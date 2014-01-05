package modules.person

import querki.ecology._
import querki.test._

class PersonTest extends QuerkiTests {
  
  "A Space" should {
    "recognize its members" in {
      val Person = getInterface[querki.identity.Person]
      implicit val s = commonState
      
      val personId = commonSpace.member1.person.id
      val person = commonState.anything(personId)
      assert(person.isDefined)
      assert(Person.hasPerson(commonSpace.member1.user, personId))
      
      assert(Person.isPerson(commonSpace.member1.user.mainIdentity, person.get))
    }
  }

}