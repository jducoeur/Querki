package querki.qtext

import org.scalatest.{FlatSpec, Matchers}
import org.junit.runner.RunWith

trait InlineBase { this:InlineParsers with Matchers =>
    def runSucceedingParsingTests(p:Parser[String], l:List[(String, String)]) {
        for ((a, b) <- l) {
            try {
                apply(p, a) should equal (b)
            } catch {
                case e:Throwable => println("Input causing the failure was: '" + a + "'."); throw e;
            }
        }
    }  
    
    def runExceptionParsingTests(p:Parser[String], l:List[String]) {
        for (s <- l) an [IllegalArgumentException] should be thrownBy{apply(p, s)}
    }
}

/**
 * Tests Inline Parsing, i.e. emphasis , strong text, links, escapes etc.
 */
//@RunWith(classOf[JUnitRunner])
class InlineParsersTest extends FlatSpec with Matchers with InlineParsers with InlineBase {

    ///////////////////////////////////////////////////////////////
    // Inline parsing Tests                                      //
    ///////////////////////////////////////////////////////////////
    val italicTests:List[(String, String)] = List(
        ("*italic*", "<em>italic</em>"),
        ("*italic * italic*", "<em>italic * italic</em>"),
        ("_italic_", "<em>italic</em>"))

    val boldTests = List(
        ("**bold**",        "<strong>bold</strong>"),
        ("**bold * bold**", "<strong>bold * bold</strong>"),
        ("__bold__",        "<strong>bold</strong>"))
        
    val strikeTests = List(
        ("--strike--",  "<strike>strike</strike>"),
        ("-- strike--", "-- strike--"))

    val codeTests = List(
        ("`code`",          "<code>code</code>"),
        ("``code``",        "<code>code</code>"),
        ("` *italic* `",    "<code> *italic* </code>"),
        ("`code\ncode`",    "<code>code\ncode</code>"),
        ("``code ` code``", "<code>code ` code</code>")
    )
    
    val classSpanTests = List(
        ("{{myClass:some stuff}}", "<span class=\"myClass\">some stuff</span>"),
        ("{{myClass:some *italic* stuff}}", "<span class=\"myClass\">some <em>italic</em> stuff</span>")
    )

    val linkTests = List(
        ("""[link text](http://example.com "link title")""",
         """<a href="http://example.com" title="link title" rel="nofollow">link text</a>"""),
        ("""[link text](http://example.com )""",
         """<a href="http://example.com" rel="nofollow">link text</a>"""),
        ("""[link text](  http://example.com  "link title"  )""",
         """<a href="http://example.com" title="link title" rel="nofollow">link text</a>"""),
        ("""[link text](  http://example.com  "li)nk" title"  )""",
         """<a href="http://example.com" title="li)nk&quot; title" rel="nofollow">link text</a>"""),
        ("""[_localName](_localName)""",
         """<a href="_localName" rel="nofollow">_localName</a>""")
    )
//
//    val fastLinkTests = List(
//        ("""<http://www.example.com?foo=a&bar=b\*>""",
//         """<a href="http://www.example.com?foo=a&amp;bar=b*">http://www.example.com?foo=a&amp;bar=b*</a>""")
//    )


    val imageTests = List(
        ("""![alt text](/src/img.png "img title")""",
         """<img src="/src/img.png" alt="alt text" title="img title" />"""),
        ("""![alt text](/src/img.png )""",
         """<img src="/src/img.png" alt="alt text" />"""),
        ("""![alt text](  /src/img.png  "img title"  )""",
         """<img src="/src/img.png" alt="alt text" title="img title" />"""),
        ("""![alt text](  /src/img.png  "i)mg" title"  )""",
         """<img src="/src/img.png" alt="alt text" title="i)mg&quot; title" />""")
    )

    val brTests = List(
        ("  \n", "<br />\n")
    )

    val xmlNameTests = List(
        ("foo",     "foo"),
        ("foo_bar", "foo_bar"),
        ("a",       "a")
    )

    val xmlNameExTests = List(
        "",
        "foo/bar",
        "foo<bar",
        "foo>bar",
        "foo\"bar",
        "foo\\bar",
        "foo bar"
    )

    val xmlStartTagTests = List(
        ("<foo>",                                           "<foo>"),
        ("""<foo attr="bar">""",                            """<foo attr="bar">"""),
        ("""<foo attr="bar" attr2="baz">""",                """<foo attr="bar" attr2="baz">"""),
        ("""<a href="http://www.example.com?p1=a&p2=b">""", """<a href="http://www.example.com?p1=a&amp;p2=b">""")
    )

    val xmlEndTagTests = List(
        ("</foo>", "</foo>"),
        ("</a>", "</a>")
    )

    val xmlInlineTests = List(
        ("""hallo <foo attr="&'<>">*italic*</foo> ballo""",
         """hallo <foo attr="&amp;&apos;&lt;&gt;"><em>italic</em></foo> ballo"""),
        ("""hallo <foo attr="&'<>"/>*italic*<foo/> ballo""",
         """hallo <foo attr="&amp;&apos;&lt;&gt;"/><em>italic</em><foo/> ballo""")
    )

    val mixedTests = List(
        ("*italic* **bold** *italic*", "<em>italic</em> <strong>bold</strong> <em>italic</em>"),
        ("*italic***bold***italic*", "<em>italic<strong>*bold</strong></em>italic*"),
        ("***foo***", "<strong><em>foo</em></strong>")
    )


    /**
     *  These should pass the inline replacement unchanged and can be used to be put between "real tests" to simualate
     *  intermediate text.
     */
    val dummyTests = List(
        ("lorem ipsum ", "lorem ipsum "),
        (" lorem ipsum", " lorem ipsum"),
        (" lorem \n ipsum ", " lorem \n ipsum ")
    )


    val allInlineTests = italicTests ++ boldTests ++ codeTests ++ linkTests ++ /* fastLinkTests ++*/ imageTests ++ brTests ++
                         strikeTests ++ /*xmlStartTagTests ++ xmlEndTagTests ++ xmlInlineTests ++*/ dummyTests

    it should "create italic text" in {
        runSucceedingParsingTests(emAsterisk(new InlineContext())|emUnderscore(new InlineContext()) , italicTests)
    }

    it should "create bold text" in {
        runSucceedingParsingTests(strongAsterisk(new InlineContext())|strongUnderscore(new InlineContext()), boldTests)
    }

    it should "create inline code" in {
        runSucceedingParsingTests(code, codeTests)
    }
    
    it should "create style spans" in {
      runSucceedingParsingTests(classSpan(new InlineContext()), classSpanTests)
    }

    it should "create links" in {
        runSucceedingParsingTests(link(new InlineContext()), linkTests)
    }

    it should "create images" in {
        runSucceedingParsingTests((elem('!')~>directImg), imageTests)
    }

    it should "create line breaks" in {
        runSucceedingParsingTests(br, brTests)
    }

//    it should "parse simplified xml identifiers" in {
//        runSucceedingParsingTests(xmlName, xmlNameTests)
//        runExceptionParsingTests(xmlName, xmlNameExTests)
//    }
//
//    it should "parse opening xml tags and escape their attribute vals" in {
//        runSucceedingParsingTests(xmlStartOrEmptyTag, xmlStartTagTests)
//    }
//
//    it should "parse closing xml tags" in {
//        runSucceedingParsingTests(xmlEndTag, xmlEndTagTests)
//    }
//
//    it should "allow inline xml and escape its parameters" in {
//        runSucceedingParsingTests(inline(Map()), xmlInlineTests)
//    }

    it should "parse mixed inline cases" in {
        runSucceedingParsingTests(inline(Map()), mixedTests)
    }

    val ld1 = new LinkDefinition("id", "http://www.example.com",     Some("Title"))
    val ld2 = new LinkDefinition("id 2", "http://other.example.com", Some("Title 2"))
    val ld3 = new LinkDefinition("id 3", "http://none.example.com",  None)
    val map = Map(ld1.id -> ld1, ld2.id -> ld2, ld3.id -> ld3)
    val ctx = new InlineContext(map)

    it should "resolve references" in {
        val p  = ref(ctx)
        apply(p, "[text][id]") should equal ((ld1, "text"))
        apply(p, "[text] [id]") should equal ((ld1, "text"))
        apply(p, "[id][]") should equal ((ld1, "id"))
        apply(p, "[id] []") should equal ((ld1, "id"))
        apply(p, "[id]") should equal ((ld1, "id"))
        apply(p, "[Id]") should equal ((ld1, "Id"))
    }

    it should "resolve reference links" in {
        val p  = inline(map)
        apply(p, "[text][id]")  should equal ("""<a href="http://www.example.com" title="Title" rel="nofollow">text</a>""")
        apply(p, "[text] [id]") should equal ("""<a href="http://www.example.com" title="Title" rel="nofollow">text</a>""")
        apply(p, "[id][]")      should equal ("""<a href="http://www.example.com" title="Title" rel="nofollow">id</a>""")
        apply(p, "[id] []")     should equal ("""<a href="http://www.example.com" title="Title" rel="nofollow">id</a>""")
        apply(p, "[id]")        should equal ("""<a href="http://www.example.com" title="Title" rel="nofollow">id</a>""")
        apply(p, "[Id]")        should equal ("""<a href="http://www.example.com" title="Title" rel="nofollow">Id</a>""")

        apply(p, "[id] [Id 2]")        should equal ("""<a href="http://other.example.com" title="Title 2" rel="nofollow">id</a>""")
        apply(p, "[id 3]")             should equal ("""<a href="http://none.example.com" rel="nofollow">id 3</a>""")
        apply(p, "[foo \"bar\"][id 3]")             should equal ("""<a href="http://none.example.com" rel="nofollow">foo &quot;bar&quot;</a>""")
    }

    it should "resolve reference images" in {
        val p  = inline(map)
        apply(p, "![text][id]")  should equal ("""<img src="http://www.example.com" alt="text" title="Title" />""")
        apply(p, "![text] [id]") should equal ("""<img src="http://www.example.com" alt="text" title="Title" />""")
        apply(p, "![id][]")      should equal ("""<img src="http://www.example.com" alt="id" title="Title" />""")
        apply(p, "![id] []")     should equal ("""<img src="http://www.example.com" alt="id" title="Title" />""")
        apply(p, "![id]")        should equal ("""<img src="http://www.example.com" alt="id" title="Title" />""")
        apply(p, "![Id]")        should equal ("""<img src="http://www.example.com" alt="Id" title="Title" />""")

        apply(p, "![id] [Id 2]")         should equal ("""<img src="http://other.example.com" alt="id" title="Title 2" />""")
        apply(p, "![id 3]")              should equal ("""<img src="http://none.example.com" alt="id 3" />""")
        apply(p, "![foo \"bar\"][id 3]") should equal ("""<img src="http://none.example.com" alt="foo &quot;bar&quot;" />""")
    }

    it should "handle all inline cases with the inline replacer" in {
        runSucceedingParsingTests(inline(Map()), allInlineTests)
        val concatTests = for (
            (a1, a2) <- allInlineTests;
            (b1, b2) <- allInlineTests;
            (c1, c2) <- allInlineTests) yield (a1+ " " + b1 + " " + c1, a2 + " " + b2 + " " +c2);

        runSucceedingParsingTests(inline(Map()), concatTests)
    }
}
