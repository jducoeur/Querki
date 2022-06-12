package querki.html

import scala.xml.XML

import org.jsoup

import models.{AnyProp, Thing}

import querki.test._
import querki.util.SafeUrl

class UITests extends QuerkiTests {

  lazy val QLTestTools = interface[querki.ql.QLTestTools]

  // === _class ===
  "_class method" should {
    "render and apply if handed a Number" in {
      class TSpace extends CommonSpace {
        val intProp = new TestProperty(Core.IntType, ExactlyOne, "Int Prop")

        val withInt = new SimpleTestThing("Int Thing", intProp(42))
      }
      val space = new TSpace

      implicit val requester = commonSpace.owner

      // Since 42 comes out as plain text, it gets wrapped in a span:
      processQText(thingAsContext[TSpace](space, _.withInt), """[[Int Prop -> _class(""myClass"")]]""") should
        equal("""<span class="myClass">42</span>""")
    }

    "throw an Exception if no param is given" in {
      implicit val requester = commonSpace.owner

      processQText(commonThingAsContext(_.instance), """[[My Optional Text._edit -> _class]]""") should
        equal(expectedWarning("UI.transform.classRequired"))
    }

    "add classes to an _edit" in {
      implicit val requester = commonSpace.owner

      val html = processQText(
        commonThingAsContext(_.instance),
        """[[My Optional Text._edit -> _class(""myClass otherClass"")]]"""
      )
      val doc = jsoup.Jsoup.parse(html).body.children.first
      val classes = doc.classNames()
      assert(classes.contains("propEditor"))
      assert(classes.contains("myClass"))
      assert(classes.contains("otherClass"))
    }

    "add classes to a text" in {
      processQText(commonThingAsContext(_.instance), """[[""hello world"" -> _class(""myClass otherClass"")]]""") should
        equal("""<span class="myClass otherClass">hello world</span>""")
    }

    "add classes to a bullet list" in {
      processQText(
        commonThingAsContext(_.instance),
        """[[""* hello
          |* world"" -> _class(""myClass otherClass"")]]""".stripMargin
      ) should
        equal("<ul class=\"myClass otherClass\"> \n <li>hello</li> \n <li>world</li> \n</ul>")
    }

    "add classes correctly to a multiparagraph text" in {
      processQText(
        commonThingAsContext(_.instance),
        """[[""hello
          |
          |world"" -> _class(""myClass otherClass"")]]""".stripMargin
      ) should
        equal("<span class=\"myClass otherClass\">hello</span>\n<span class=\"myClass otherClass\">world</span>")
    }

    "work with an ampersand in a Tag" in {
      class TSpace extends CommonSpace {
        val tagThing = new SimpleTestThing("Tag Thing", setTagsProp("Branch & Claw"))
      }
      implicit val s = new TSpace

      pql("""[[Tag Thing -> My Set of Tags -> ""____"" -> _class(""myClass"")]]""") should
        equal("""<span class="myClass"><a href="Branch+%26+Claw">Branch &amp; Claw</a></span>""")
    }
  }

  def testBackLinkWith(
    cmd: String,
    parent: Thing,
    prop: AnyProp,
    f: Thing => String
  ) = {
    processQText(
      commonThingAsContext(_ => parent),
      s"""[[My Model -> ${prop.displayName}.$cmd(""Create"")]]""",
      Some(parent)
    ) should
      include(s"""v-${prop.id.toString}-=${f(parent)}""")
  }

  // === _createButton ===
  "_createButton" should {
    def testWith(
      parent: Thing,
      prop: AnyProp
    )(implicit
      f: Thing => String
    ) = testBackLinkWith("_createButton", parent, prop, f)

    // Note that these tests are a bit unrealistic: we're using Properties that don't actually exist on the
    // target Model, so they'd fail when you press the button. But the code doesn't currently sanity-check
    // that.
    "work with Links" in {
      implicit val f: Thing => String = _.id.toThingId.toString

      testWith(commonSpace.instance, commonSpace.singleLinkProp)
      testWith(commonSpace.instance, commonSpace.optLinkProp)
      testWith(commonSpace.instance, commonSpace.listLinksProp)
      testWith(commonSpace.instance, commonSpace.setLinksProp)
    }

    "work with Tags" in {
      implicit val f: Thing => String = { t => SafeUrl(t.displayName) }

      testWith(commonSpace.instance, commonSpace.singleTagProp)
      testWith(commonSpace.instance, commonSpace.optTagProp)
      testWith(commonSpace.instance, commonSpace.listTagsProp)
      testWith(commonSpace.instance, commonSpace.setTagsProp)
    }
  }

  // === _createInstanceLink ===
  "_createInstanceLink" should {
    def testWith(
      parent: Thing,
      prop: AnyProp
    )(implicit
      f: Thing => String
    ) = testBackLinkWith("_createInstanceLink", parent, prop, f)

    // Note that these tests are a bit unrealistic: we're using Properties that don't actually exist on the
    // target Model, so they'd fail when you press the button. But the code doesn't currently sanity-check
    // that.
    "work with Links" in {
      implicit val f: Thing => String = _.id.toThingId.toString

      testWith(commonSpace.instance, commonSpace.singleLinkProp)
      testWith(commonSpace.instance, commonSpace.optLinkProp)
      testWith(commonSpace.instance, commonSpace.listLinksProp)
      testWith(commonSpace.instance, commonSpace.setLinksProp)
    }

    "work with Tags" in {
      implicit val f: Thing => String = { t => SafeUrl(t.displayName) }

      testWith(commonSpace.instance, commonSpace.singleTagProp)
      testWith(commonSpace.instance, commonSpace.optTagProp)
      testWith(commonSpace.instance, commonSpace.listTagsProp)
      testWith(commonSpace.instance, commonSpace.setTagsProp)
    }
  }

  // === _data ===
  "_data method" should {
    "add a simple data attribute to a span" in {
      processQText(
        commonThingAsContext(_.instance),
        """[[""hello world"" -> _data(""foo"",""I am some data"")]]"""
      ) should
        equal("""<span data-foo="I am some data">hello world</span>""")
    }

    "add a simple data attribute to a div" in {
      processQText(
        commonThingAsContext(_.instance),
        """[[""{{myClass:
          |hello world
          |}}"" -> _data(""foo"",""I am some data"")]]""".stripMargin
      ) should
        equal("""<div class="myClass" data-foo="I am some data"> 
            | <span>hello world</span>
            |</div>""".stripReturns)
    }

    "cope with ampersands" in {
      implicit val s = commonSpace

      pql("""[[""hello&apos;s & world &amp; stuff"" -> _data(""foo"",""I am some data"")]]""") should
        equal("""<span data-foo="I am some data">hello's &amp; world &amp; stuff</span>""")
    }
  }

  // === _linkButton ===
  "_linkButton" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_linkButton(""hello"")]]""") should
        equal("""<a class="btn btn-primary" href="Sandbox">hello</a>""")
    }
    "work with an external URL" in {
      processQText(commonThingAsContext(_.withUrl), """[[My Optional URL -> _linkButton(""hello"")]]""") should
        equal("""<a class="btn btn-primary" href="http://www.google.com/" target="_blank">hello</a>""")
    }
    "quietly ignore an empty context" in {
      processQText(commonThingAsContext(_.withoutUrl), """[[My Optional URL -> _linkButton(""hello"")]]""") should
        equal("")
    }
  }

  // === _propLink ===
  "_propLink" should {
    "work normally" in {
      implicit val s = new CDSpace

      pql("""[[My Favorites -> Show Favorites._propLink -> ""__Faves__""]]""") should
        equal("[Faves](My-Favorites?prop=Show-Favorites)")
    }
  }

  // === _QLButton ===
  "_QLButton" should {
    "produce the right button" in {
      implicit val s = commonSpace
      implicit val state = s.state

      val context = ExactlyOne(Core.LinkType(s.instance))
      val serialized = QLTestTools.serializeContextCore(context, Map.empty)

      pql("""[[My Instance -> _QLButton(""Label"", My Optional Text, ""myTarget"")]]""") should
        equal(
          s"""<a class="btn btn-primary _qlInvoke" data-ptype="${Core.LinkType.id.toThingId}" data-context=".$serialized" data-target="myTarget" data-ql="My Optional Text" data-append="false" data-replace="false" data-noicon="false" data-updatesection="false"  href="#" >Label</a>"""
        )
    }

    "works with multiple items in context" in {
      implicit val s = commonSpace
      implicit val state = s.state

      val context = QList.makePropValue(List(Core.LinkType(s.instance), Core.LinkType(state)), Core.LinkType)
      val serialized = QLTestTools.serializeContextCore(context, Map.empty)

      pql("""[[<My Instance, _space> -> _QLButton(""Label"", My Optional Text, ""myTarget"")]]""") should
        equal(
          s"""<a class="btn btn-primary _qlInvoke" data-ptype="${Core.LinkType.id.toThingId}" data-context=".$serialized" data-target="myTarget" data-ql="My Optional Text" data-append="false" data-replace="false" data-noicon="false" data-updatesection="false"  href="#" >Label</a>"""
        )
    }

    "use append properly" in {
      implicit val s = commonSpace
      implicit val state = s.state

      val context = ExactlyOne(Core.LinkType(s.instance))
      val serialized = QLTestTools.serializeContextCore(context, Map.empty)

      pql("""[[My Instance -> _QLButton(""Label"", My Optional Text, ""myTarget"", append=True)]]""") should
        equal(
          s"""<a class="btn btn-primary _qlInvoke" data-ptype="${Core.LinkType.id.toThingId}" data-context=".$serialized" data-target="myTarget" data-ql="My Optional Text" data-append="true" data-replace="false" data-noicon="false" data-updatesection="false"  href="#" >Label</a>"""
        )
    }

    "work with a bound name" in {
      implicit val s = commonSpace
      implicit val state = s.state

      val context = ExactlyOne(Core.LinkType(state))
      val binding = ExactlyOne(Core.LinkType(s.instance))
      val serialized = QLTestTools.serializeContextCore(context, Map("bound" -> binding))

      pql("""[[My Instance -> +$bound 
        _QLButton(label=""Label"", ql=$bound, target=""myTarget"")]]""") should
        equal(
          s"""<a class="btn btn-primary _qlInvoke" data-ptype="${Core.LinkType.id.toThingId}" data-context=".$serialized" data-target="myTarget" data-ql="$$bound" data-append="false" data-replace="false" data-noicon="false" data-updatesection="false"  href="#" >Label</a>"""
        )
    }
  }

  // === _QLInput ===
  "_QLInput" should {
    "render properly" in {
      implicit val s = commonSpace

      pql("""[[_QLInput(label=""Hello"", ql=""[[$input]]"")]]""") should
        include(s"""placeholder="Hello"""")
    }
  }

  // === _section ===
  "_section" should {
    "work normally" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, QList, "Numbers")

        val myThing = new SimpleTestThing("Test Thing", numProp(1, 2, 3, 4, 5))
      }
      implicit val s = new TSpace

      pql("""[[Test Thing -> Numbers -> _section(""Nums: "", ""____"")]]""") should
        equal("""
      Nums: 
      
      1
      2
      3
      4
      5""".strip)
    }

    "work with an empty list" in {
      implicit val s = commonSpace
      pql("""The result is [[Trivial._instances -> _section(""Inner Header"", _bulleted)]]""") should
        equal("""The result is """)
    }
  }

  // === _showLink ===
  "_showLink" should {
    "work with a Link to Thing" in {
      processQText(commonThingAsContext(_.sandbox), """[[_showLink(""hello"")]]""") should
        equal("""<a href="Sandbox">hello</a>""")
    }
    "not over-escape HTML entities in labels" in {
      processQText(commonThingAsContext(_.sandbox), """[[_showLink(""I'm the label"")]]""") should
        equal("""<a href="Sandbox">I'm the label</a>""")
    }
    "work with an external URL" in {
      processQText(commonThingAsContext(_.withUrl), """[[My Optional URL -> _showLink(""hello"")]]""") should
        equal("""<a href="http://www.google.com/" target="_blank">hello</a>""")
    }
    "quietly ignore an empty context" in {
      processQText(commonThingAsContext(_.withoutUrl), """[[My Optional URL -> _showLink(""hello"")]]""") should
        equal("")
    }
    "work with a list of external URLs" in {
      // Note that a List result will have newlines in the QText, intentionally:
      processQText(commonThingAsContext(_.withUrl), """[[My List of URLs -> _showLink(""hello"")]]""") should
        equal("""
          |<a href="http://www.google.com/" target="_blank">hello</a>
          |<a href="http://querki.net/" target="_blank">hello</a>""".stripReturns)
    }
    "work with a list of Links" in {
      class testSpace extends CommonSpace {
        val withLinks = new SimpleTestThing("With Links", listLinksProp(sandbox.id, withUrlOID))
      }

      processQText(
        thingAsContext[testSpace](new testSpace, _.withLinks),
        """[[My List of Links -> _showLink(Link Name)]]"""
      ) should
        equal("""
          |<a href="Sandbox">Sandbox</a>
          |<a href="With-URL">With URL</a>""".stripReturns)
    }
  }

  // === _showSome ===
  "_showSome function" should {
    "work normally" in {
      class TSpace extends CommonSpace {
        val numProp = new TestProperty(Core.IntType, QList, "Numbers")

        val myThing = new SimpleTestThing("Test Thing", numProp(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
      }
      implicit val s = new TSpace

      pql("""[[Test Thing -> _showSome(3,4,""More"",Numbers,_commas)]]""") should
        include("3, 4, 5, 6")
    }
  }

  // === _tooltip ===
  "_tooltip method" should {
    "add a tooltip to a text block" in {
      processQText(commonThingAsContext(_.instance), """[[""hello world"" -> _tooltip(""I am a tooltip"")]]""") should
        equal("""<span class="_withTooltip" title="I am a tooltip">hello world</span>""")
    }
  }
}
