package eu.henkelmann.actuarius

import querki.test._

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class HtmlTest extends QuerkiTests with Transformer {
  "Simple, legal XHTML fragments" should {
    "parse cleanly" in {
      apply("Hello <div>there</div>") should equal ("""<p>Hello <div>there</div></p>
        |""".stripReturns)
      apply("Hello <span>there</span>") should equal ("""<p>Hello <span>there</span></p>
        |""".stripReturns)
    }    
  }
  
  "HTML blocks" should {
    "work correctly now" in {
      apply("""<div class="effects-box">

Foo foo foo

Foo foo

</div>""".stripReturns) should equal ("""<div class="effects-box">
<p>Foo foo foo</p>
<p>Foo foo</p>
</div>
    |""".stripReturns)
    }
  }
  
  "Legal class attributes" should {
    "parse cleanly" in {
      apply("""Hello <div class="class-1 class_2">there</div>""") should equal ("""<p>Hello <div class="class-1 class_2">there</div></p>
        |""".stripReturns)
      apply("""Hello <span class="class-1 class_2">there</span>""") should equal ("""<p>Hello <span class="class-1 class_2">there</span></p>
        |""".stripReturns)
    }    
  }
  
  "Data attributes" should {
    "parse if single-valued" in {
      apply("""Hello <div data-thingy="firstValue">there</div>""") should equal ("""<p>Hello <div data-thingy="firstValue">there</div></p>
        |""".stripReturns)
    }
    "parse if multi-valued" in {
      apply("""Hello <div data-thingy="firstValue,secondValue">there</div>""") should equal ("""<p>Hello <div data-thingy="firstValue,secondValue">there</div></p>
        |""".stripReturns)
    }
    "parse with single quotes" in {
      apply("""Hello <div data-thingy='firstValue'>there</div>""") should equal ("""<p>Hello <div data-thingy='firstValue'>there</div></p>
        |""".stripReturns)
    }
    "be escaped on bad name" in {
      apply("""Hello <div data-foo!="something">there</div>""") should equal ("""<p>Hello &lt;div data-foo!=&quot;something&quot;&gt;there</div></p>
        |""".stripReturns)      
    }
  }
  
  "Legal id attributes" should {
    "parse cleanly" in {
      apply("""Hello <div id="foo">there</div>""") should equal ("""<p>Hello <div id="foo">there</div></p>
        |""".stripReturns)
    }    
  }
  
  "Illegal Tags" should {
    "be escaped and passed through" in {
      apply("Hello <script>there</script>") should equal ("""<p>Hello &lt;script&gt;there&lt;/script&gt;</p>
        |""".stripReturns)      
    }
  }
  
  "Illegal attributes" should {
    "be escaped and passed through" in {
      apply("""Hello <div style="something">there</div>""") should equal ("""<p>Hello &lt;div style=&quot;something&quot;&gt;there</div></p>
        |""".stripReturns)      
    }
  }
  
  "Illegal attribute values" should {
    "be escaped and passed through" in {
      apply("""Hello <div class="javascript:stuff">there</div>""") should equal ("""<p>Hello &lt;div class=&quot;javascript:stuff&quot;&gt;there</div></p>
        |""".stripReturns)      
    }    
  }
  
  "Pure HTML lines" should {
    "parse with the HTML at start of line" in {
      apply("""My initial content
          |
          |<div class="thingy">
          |
          |Stuff in the div
          |
          |</div>
          |
          |content after the div.""".stripReturns) should equal ("""<p>My initial content</p>
          |<div class="thingy">
          |<p>Stuff in the div</p>
          |</div>
          |<p>content after the div.</p>
          |""".stripReturns)
    }
    
    "parse with the HTML inset various amounts" in {
      apply("""My initial content
          |
          |<div class="thingy">
          |  <div class="subdiv">
          |        <div class="deepdiv">
          |
          |Stuff in the div
          |
          |        </div>
          |  </div>
          |</div>
          |
          |content after the div.""".stripReturns) should equal("""<p>My initial content</p>
          |<div class="thingy">
          |  <div class="subdiv">
          |        <div class="deepdiv">
          |<p>Stuff in the div</p>
          |        </div>
          |  </div>
          |</div>
          |<p>content after the div.</p>
          |""".stripReturns)
    }
    
    "parse multiple HTML tags on a line" in {
      apply("""My initial content
          |
          |  <div class="thingy">  <div class="subdiv">
          |        <div class="deepdiv">
          |
          |Stuff in the div
          |
          |        </div>
          |  </div>  </div>
          |
          |content after the div.""".stripReturns) should equal("""<p>My initial content</p>
          |  <div class="thingy"><div class="subdiv">
          |        <div class="deepdiv">
          |<p>Stuff in the div</p>
          |        </div>
          |  </div></div>
          |<p>content after the div.</p>
          |""".stripReturns)
    }
    
    "Cope correctly when run-on with a paragraph" in {
      apply("""My initial content
          |<div class="thingy">
          |  <div class="subdiv">
          |        <div class="deepdiv">
          |Stuff in the div
          |        </div>
          |  </div>
          |</div>
          |content after the div.""".stripReturns) should equal("""<p>My initial content</p>
          |<div class="thingy">
          |  <div class="subdiv">
          |        <div class="deepdiv">
          |<p>Stuff in the div</p>
          |        </div>
          |  </div>
          |</div>
          |<p>content after the div.</p>
          |""".stripReturns)
    }
  }
}