package querki.qtext

import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}

/**
 * Tests the parsing on block level.
 */
//@RunWith(classOf[JUnitRunner])
class BlockParsersTest extends FlatSpec with Matchers with BlockParsers{
  
  def deco() = new MainDecorator {}

    "The BlockParsers" should "parse optional empty lines" in {
        val p = optEmptyLines
        val el = new EmptyLine(" \n")
        apply(p, Nil)   should equal (Nil)
        apply(p, List(el)) should equal (List(el))
        apply(p, List(el, el)) should equal (List(el, el))
    }

    it should "accept empty documents" in {
        val p = markdown
        val el = new EmptyLine(" \n")
        apply(p, Nil)   should equal (Nil)
        apply(p, List(el)) should equal (Nil)
        apply(p, List(el, el)) should equal (Nil)
    }

    it should "detect line types" in {
        val p = line(classOf[CodeLine])
        apply(p, List(new CodeLine("    ", "code"))) should equal (new CodeLine("    ", "code"))
        an [IllegalArgumentException] should be thrownBy(apply(p, List(new OtherLine("foo"))))
    }
    
    it should "cope with HTML" in {
      val p = markdown
      val lines = List(
          OtherLine("My initial content"),
          EmptyLine(""),
          XmlChunk("""<div class="thingy">
              |""".stripMargin),
          EmptyLine(""),
          OtherLine("Stuff in the div"),
          EmptyLine(""),
          XmlChunk("""</div>
              |""".stripMargin),
          EmptyLine(""),
          OtherLine("content after the div.")
          )
      val result = apply(p, lines)
      
      // TODO: this should properly test the result!
//      println(result)
    }
}