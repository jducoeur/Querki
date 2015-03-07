package querki.qtext

import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith

/**
 * tests parsing of individual lines
 */
//@RunWith(classOf[JUnitRunner])
class LineParsersTest extends FlatSpec with Matchers with LineParsers{

    "The LineParsers" should "parse horizontal rulers" in {
        val p = ruler
        apply(p, "---")           should equal (new RulerLine("---"))
        apply(p, "***")           should equal (new RulerLine("***"))
        apply(p, "---------")     should equal (new RulerLine("---------"))
        apply(p, "*********")     should equal (new RulerLine("*********"))
        apply(p, "- - -")         should equal (new RulerLine("- - -"))
        apply(p, "* * *")         should equal (new RulerLine("* * *"))
        apply(p, "  ---")         should equal (new RulerLine("  ---"))
        apply(p, "  ***")         should equal (new RulerLine("  ***"))
        apply(p, "  - - -  ----") should equal (new RulerLine("  - - -  ----"))
        apply(p, "  * * *  ****") should equal (new RulerLine("  * * *  ****"))
    }

    it should "parse a ruler that starts like a setext header line" in {
        val p = setext2OrRuler
        apply(p, "- - -")         should equal (new RulerLine("- - -"))
    }

    it should "parse a setext style level 1 header underline" in {
        val p = setextHeader1
        apply(p, "=") should equal (new SetExtHeaderLine("=", 1))
        apply(p, "== ") should equal (new SetExtHeaderLine("== ", 1))
        apply(p, "========== \t ") should equal (new SetExtHeaderLine("========== \t ", 1))
    }

    it should "parse a setext style level 2 header underline" in {
        val p = setextHeader2
        apply(p, "-") should equal (new SetExtHeaderLine("-", 2))
        apply(p, "-- ") should equal (new SetExtHeaderLine("-- ", 2))
        apply(p, "---------- \t ") should equal (new SetExtHeaderLine("---------- \t ", 2))
    }

    it should "parse an atx header line" in {
        val p = atxHeader
        apply(p, "#foo") should equal (new AtxHeaderLine("#", "foo"))
        apply(p, "## #foo##") should equal (new AtxHeaderLine("##", " #foo##"))
    }

    it should "parse an empty line" in {
        val p = emptyLine
        apply (p, "") should equal (new EmptyLine(""))
        apply (p, "  \t ") should equal (new EmptyLine("  \t "))
        an [IllegalArgumentException] should be thrownBy (apply (p, " not empty "))
    }

    it should "parse arbitrary lines as OtherLine tokens" in {
        val p = otherLine
        apply(p, "a line")          should equal (new OtherLine("a line"))
    }

    it should "parse quoted block lines" in {
        val p = blockquoteLine
        apply(p, "> quote") should equal (new BlockQuoteLine("> ", "quote"))
        apply(p, ">     codequote") should equal (new BlockQuoteLine("> ", "    codequote"))
        apply(p, "   >     codequote") should equal (new BlockQuoteLine("   > ", "    codequote"))
        an [IllegalArgumentException] should be thrownBy(apply(p, "not a quote")) 
    }

    it should "parse unordered item start lines" in {
        val p = uItemStartLine
        apply(p, "* foo") should equal (new UItemStartLine("* ", "foo"))
        apply(p, " * foo") should equal (new UItemStartLine(" * ", "foo"))
        apply(p, "  * foo") should equal (new UItemStartLine("  * ", "foo"))
        apply(p, "   * foo") should equal (new UItemStartLine("   * ", "foo"))
        apply(p, "   *    foo") should equal (new UItemStartLine("   *    ", "foo"))
        apply(p, "   * \t  foo") should equal (new UItemStartLine("   * \t  ", "foo"))
        apply(p, "   * \t  foo  ") should equal (new UItemStartLine("   * \t  ", "foo  "))

        an [IllegalArgumentException] should be thrownBy(apply(p, "*foo"))
        an [IllegalArgumentException] should be thrownBy(apply(p, "    * foo"))
        an [IllegalArgumentException] should be thrownBy(apply(p, "1. foo"))

        apply(p, "* foo") should equal (new UItemStartLine("* ", "foo"))
        apply(p, "+ foo") should equal (new UItemStartLine("+ ", "foo"))
        apply(p, "- foo") should equal (new UItemStartLine("- ", "foo"))
    }


    it should "parse ordered item start lines" in {
        val p = oItemStartLine
        apply(p, "1. foo") should equal (OItemStartLine("1. ", "foo"))
        apply(p, " 12. foo") should equal (OItemStartLine(" 12. ", "foo"))
        apply(p, "  0. foo") should equal (OItemStartLine("  0. ", "foo"))
        apply(p, "   44444444. foo") should equal (OItemStartLine("   44444444. ", "foo"))
        apply(p, "   465789.    foo") should equal (OItemStartLine("   465789.    ", "foo"))
        apply(p, "   4455. \t  foo") should equal (OItemStartLine("   4455. \t  ", "foo"))
        apply(p, "   9. \t  foo  ") should equal (OItemStartLine("   9. \t  ", "foo  "))

        an [IllegalArgumentException] should be thrownBy(apply(p, "1.foo"))
        an [IllegalArgumentException] should be thrownBy(apply(p, "    1. foo"))
        an [IllegalArgumentException] should be thrownBy(apply(p, "* foo"))
    }
    
    it should "parse definition start lines" in {
      val p = dItemStartLine
      apply(p, ": nam : def") should equal (DItemStartLine(": nam : ", "def", "nam"))
      apply(p, " : This is My Title! : And This is My Definition") should equal (
          DItemStartLine(" : This is My Title! : ", "And This is My Definition", "This is My Title!"))
      apply(p, " : Space: The Final Frontier : These are the voyages") should equal (
          DItemStartLine(" : Space: The Final Frontier : ", "These are the voyages", "Space: The Final Frontier"))      
    }
    
    it should "parse class div declarations" in {
      val p = classDivStart
      apply(p, "{{myClass:") should equal (
          ClassDivStartLine("{{myClass:", "myClass"))
      apply(p, "{{ myClass :") should equal (
          ClassDivStartLine("{{ myClass :", "myClass"))
      apply(p, "{{ class-with-dashes :") should equal (
          ClassDivStartLine("{{ class-with-dashes :", "class-with-dashes"))
      
      // If there is more on the line, then it is a classSpan instead:
      an [IllegalArgumentException] should be thrownBy(apply(p, "{{ myClass : some span text"))
      
      val p2 = classDivEnd
      apply(p2, "}}") should equal(ClassDivEnd("}}"))
      apply(p2, "  }}") should equal(ClassDivEnd("  }}"))
    }

    it should "parse link definitions" in {
        val p = linkDefinitionStart
        apply(p, "[foo]: http://example.com/  \"Optional Title Here\"") should equal (
        new LinkDefinitionStart("foo", "http://example.com/"), Some("Optional Title Here"))
        apply(p, "[foo]: http://example.com/") should equal (
        new LinkDefinitionStart("foo", "http://example.com/"), None)
        apply(p, "[Foo]: http://example.com/  'Optional Title Here'") should equal (
        new LinkDefinitionStart("foo", "http://example.com/"), Some("Optional Title Here"))
        apply(p, "[Foo]: http://example.com/?bla=<>  (Optional Title Here)") should equal (
        new LinkDefinitionStart("foo", "http://example.com/?bla=&lt;&gt;"), Some("Optional Title Here"))
        apply(p, "[Foo]: http://example.com/?bla=<>  (Optional Title Here)") should equal (
        new LinkDefinitionStart("foo", "http://example.com/?bla=&lt;&gt;"), Some("Optional Title Here"))
    }

    it should "parse link titles" in {
        val p = linkDefinitionTitle
        apply(p, "  (Optional Title Here)  ") should equal ("Optional Title Here")
    }
    
    it should "parse openings of fenced code blocks" in {
      val p = fencedCodeStartOrEnd
      apply(p, "```") should equal (
      new FencedCode("```"))
      apply(p, "   ```\t") should equal (
      new FencedCode("   ```\t"))
      apply(p, "  ``` \t ") should equal (
      new FencedCode("  ``` \t "))
      apply(p, "  ``` \t java  \t ") should equal (
      new ExtendedFencedCode("  ``` \t ", "java  \t "))
    }
}