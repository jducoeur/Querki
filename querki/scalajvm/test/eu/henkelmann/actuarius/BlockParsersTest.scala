package eu.henkelmann.actuarius

import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

/**
 * Tests the parsing on block level.
 */
//@RunWith(classOf[JUnitRunner])
class BlockParsersTest extends FlatSpec with ShouldMatchers with BlockParsers{

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
        evaluating(apply(p, List(new OtherLine("foo")))) should produce[IllegalArgumentException]
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
      println(result)
    }
}