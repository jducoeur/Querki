package querki.test

import models.{ThingState}
import models.Kind

import querki.ecology._
import querki.identity.{User}
import querki.identity.UserLevel._

/**
 * A generic Space, with a grab-bag of helpful components, for using in various tests.
 *
 * Basically, if you have a Property or Thing that you expect to use in a bunch of
 * tests, put them in here. If you are only using them in one or two tests, subclass
 * this and use that instead.
 */
class CommonSpace(implicit ecologyIn: Ecology) extends TestSpace {

  def ecology = ecologyIn

  lazy val Links = interface[querki.links.Links]
  lazy val Tags = interface[querki.tags.Tags]

  lazy val ExternalLinkType = Links.URLType
  lazy val TextType = Core.TextType
  lazy val LinkType = Core.LinkType
  lazy val TagType = Tags.NewTagSetType

  // In the simple case, we only have one Space, so it can own the World:
  lazy val world = new TestWorld

  /**
   * *********************************************
   * PROPERTIES
   * *********************************************
   */

  val singleLinkProp = new TestProperty(LinkType, ExactlyOne, "Single Link")
  val optLinkProp = new TestProperty(LinkType, Optional, "Optional Link")
  val listLinksProp = new TestProperty(LinkType, QList, "My List of Links")
  val setLinksProp = new TestProperty(LinkType, QSet, "My Set of Links")

  val singleTagProp = new TestProperty(TagType, ExactlyOne, "Single Tag")
  val optTagProp = new TestProperty(TagType, Optional, "Optional Tag")
  val listTagsProp = new TestProperty(TagType, QList, "My List of Tags")
  val setTagsProp = new TestProperty(TagType, QSet, "My Set of Tags")

  val listURLProp = new TestProperty(ExternalLinkType, QList, "My List of URLs")
  val optURLProp = new TestProperty(ExternalLinkType, Optional, "My Optional URL")

  val singleTextProp = new TestProperty(TextType, ExactlyOne, "Single Text")
  val optTextProp = new TestProperty(TextType, Optional, "My Optional Text")

  /**
   * *********************************************
   * THINGS
   * *********************************************
   */

  // Some Members and non-Members
  val member1 = member("Some Member", "memberHandle", PaidUser)
  val member2 = member("Another Member", "member2Handle", PaidUser)
  val nonMember: User = userAs("Non-Member", "nonMemberHandle", PaidUser)

  // A simple default Model and Instance.
  val testModel = new SimpleTestThing("My Model", Core.IsModelProp(true))
  val instance = new TestThing("My Instance", testModel, optTextProp("Hello world"))

  val withDisplayName = new SimpleTestThing(
    "Interesting Display Name",
    interface[querki.basic.Basic].DisplayNameProp("""My name is "interesting"!""")
  )
  val trivialThing = new SimpleTestThing("Trivial")

  /**
   * The generic "sandbox" Thing, which serves as a useful default if you don't
   * care much about the details.
   */
  val sandbox = new SimpleTestThing("Sandbox")

  lazy val withUrlOID = toid()

  val withUrl = new TestThing(
    withUrlOID,
    "With URL",
    optURLProp("http://www.google.com/"),
    listURLProp("http://www.google.com/", "http://www.querki.net/")
  )

  val withoutUrl = new SimpleTestThing("Without URL", optURLProp())
}
