package querki.test

import models.{Attachment, ThingState}
import models.Kind
import models.system.{ExternalLinkType, LinkType}
import models.system.{ExactlyOne, Optional, QList}
import models.system.{IsModelProp}
import models.system.OIDs.PageOID

import querki.identity.{User}
import querki.identity.UserLevel._

/**
 * A generic Space, with a grab-bag of helpful components, for using in various tests.
 * 
 * Basically, if you have a Property or Thing that you expect to use in a bunch of
 * tests, put them in here. If you are only using them in one or two tests, subclass
 * this and use that instead.
 */
class CommonSpace extends TestSpace {
  // In the simple case, we only have one Space, so it can own the World:
  val world = new TestWorld
  
  /***********************************************
   * PROPERTIES
   ***********************************************/
  
  lazy val listLinksProp = new TestProperty(LinkType, QList, "My List of Links")
  lazy val listURLProp = new TestProperty(ExternalLinkType, QList, "My List of URLs")
  
  lazy val optURLProp = new TestProperty(ExternalLinkType, Optional, "My Optional URL")
  
  lazy val singleLinkProp = new TestProperty(LinkType, ExactlyOne, "Single Link")
  
  override lazy val props = Seq(
    listLinksProp,
    listURLProp,
    optURLProp,
    singleLinkProp
  )

  /***********************************************
   * THINGS
   ***********************************************/

  // Some Members and non-Members
  val member1 = member("Some Member", "memberHandle", PaidUser)
  val member2 = member("Another Member", "member2Handle", PaidUser)
  val nonMember:User = userAs("Non-Member", "nonMemberHandle", PaidUser)
  
  // A simple default Model and Instance.
  val testModel = new SimpleTestThing("My Model", IsModelProp(true))
  val instance = new TestThing("My Instance", testModel) 
  
  /**
   * A simple imitation "photograph".
   */
  val photo = new Attachment(toid(), spaceId, PageOID, makePropFetcher("My Photo", Seq.empty))
  registerThing(photo)

  /**
   * The generic "sandbox" Thing, which serves as a useful default if you don't
   * care much about the details.
   */
  val sandbox = new SimpleTestThing("Sandbox")
     
  lazy val withUrlOID = toid()
  val withUrl = new TestThing(withUrlOID, "With URL",
      optURLProp("http://www.google.com/"),
      listURLProp("http://www.google.com/", "http://www.querki.net/"))
  
  val withoutUrl = new SimpleTestThing("Without URL", optURLProp())
}

