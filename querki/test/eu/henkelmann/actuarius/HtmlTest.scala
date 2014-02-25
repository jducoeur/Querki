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
  
  "Legal class attributes" should {
    "parse cleanly" in {
      apply("""Hello <div class="class-1 class_2">there</div>""") should equal ("""<p>Hello <div class="class-1 class_2">there</div></p>
        |""".stripReturns)
      apply("""Hello <span class="class-1 class_2">there</span>""") should equal ("""<p>Hello <span class="class-1 class_2">there</span></p>
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
}