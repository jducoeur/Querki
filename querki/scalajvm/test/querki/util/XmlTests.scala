package querki.util

import scala.xml._

import querki.test._

class XmlTests extends QuerkiTests {
  "parseXhtmlFragment" should {
    "handle a single node" in {
      val nodes = XmlHelpers.parseXhtmlFragment("<span>Hello</span>")

      assert(nodes.length == 1)
      XmlHelpers.mapElems(nodes) { elem =>
        assert(elem.label == "span")
        val content = elem.child.head
        assert(content.isInstanceOf[Text])
        val text = content.asInstanceOf[Text]
        assert(text.text == "Hello")
        elem
      }
    }

    "handle two simple nodes" in {
      val nodes = XmlHelpers.parseXhtmlFragment("<span>Hello</span><span>there</span>")

      assert(nodes.length == 2)
      val node = nodes.tail.head
      val elem = node.asInstanceOf[Elem]
      assert(elem.label == "span")
      val content = elem.child.head
      assert(content.isInstanceOf[Text])
      val text = content.asInstanceOf[Text]
      assert(text.text == "there")
    }

    "handle two nodes with other stuff" in {
      val nodes = XmlHelpers.parseXhtmlFragment("""
          |<span>Hello</span>
          |    <span>there</span>
          |    <span>dude</span>
          |""".stripReturns)

      assert(nodes.length == 3)
      val node = nodes.tail.head
      val elem = node.asInstanceOf[Elem]
      assert(elem.label == "span")
      val content = elem.child.head
      assert(content.isInstanceOf[Text])
      val text = content.asInstanceOf[Text]
      assert(text.text == "there")
    }
  }
}
