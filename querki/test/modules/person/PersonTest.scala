package modules.person

import querki.test._

class PersonTest extends QuerkiTests {
  
  "A Space" should {
    "recognize its members" in {
      import modules.person.PersonModule._
      implicit val s = commonState
      
      val personId = commonSpace.member1.person.id
      val person = commonState.anything(personId)
      assert(person.isDefined)
      assert(commonSpace.member1.user.hasPerson(personId))
    }
  }

}