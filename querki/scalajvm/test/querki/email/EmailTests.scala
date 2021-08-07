package querki.email

import models.Thing
import querki.test._
import querki.util.SafeUrl

class EmailTests extends QuerkiTests {

  class EmailSpace extends CommonSpace {
    val email = new TestProperty(TextType, Optional, "Email Address")

    val recipient = new TestModel("Recipient", email())
    val joe = new TestThing("Joe", recipient, email("joe@place.net"))
    val roger = new TestThing("Roger", recipient, email("roger@place.net"))
    val bob = new TestThing("Bob", recipient, email("bob@place.net"))
    val alice = new TestThing("Alice", recipient, email("alice@place.net"))
    val denise = new TestThing("Denise", recipient, email("denise@place.net"))
    val marcus = new TestThing("Marcus", recipient, email("marcus@place.net"))
    val marie = new TestThing("Marie", recipient, email("marie@place.net"))
  }

  def link(url: String) = s"[$url]($url)"
  def safe(s: String): String = SafeUrl(s)

  def to(ts: Thing*)(implicit s: EmailSpace): String = {
    implicit val state = s.state

    val emailProp = s.email
    ts.map { t =>
      safe(t.getPropAll(emailProp).head.text)
    }.mkString(",")
  }

  "_emailLink" should {
    "produce an empty window if no params" in {
      implicit val s = new EmailSpace

      pql("""[[_emailLink]]""") should equal(link("mailto:?"))
    }

    "work with one recipient" in {
      implicit val s = new EmailSpace

      pql("""[[Joe -> _emailLink(to=Email Address)]]""") should equal(link(s"mailto:${to(s.joe)}?"))
    }

    "work with several recipients" in {
      implicit val s = new EmailSpace

      pql("""[[<Joe, Alice, Marie> -> _emailLink(to=Email Address)]]""") should
        equal(link(s"mailto:${to(s.joe, s.alice, s.marie)}?"))
    }

    "work with Cc:s and Bcc:s" in {
      implicit val s = new EmailSpace

      pql("""[[<Joe, Alice, Marie> -> 
            | _emailLink(
            |   to=Email Address,
            |   cc=<Roger, Bob> -> Email Address,
            |   bcc=<Denise, Marcus> -> Email Address)]]""".stripMargin) should
        equal(link(s"mailto:${to(s.joe, s.alice, s.marie)}?cc=${to(s.roger, s.bob)}&bcc=${to(s.denise, s.marcus)}"))
    }

    "work with subject and body" in {
      implicit val s = new EmailSpace

      pql("""[[Joe -> _emailLink(to=Email Address, subject=""Hi there!"", body=""Some content here"")]]""") should
        equal(link(s"mailto:${to(s.joe)}?subject=${safe("Hi there!")}&body=${safe("Some content here\n\n")}"))
    }

  }
}
