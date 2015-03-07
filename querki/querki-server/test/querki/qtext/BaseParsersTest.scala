package querki.qtext
import org.scalatest.{FlatSpec, Matchers}
import collection.SortedMap
import org.junit.runner.RunWith

/**
 * Tests basic parsers that are used by the more complex parsing steps.
 */

//@RunWith(classOf[JUnitRunner])
class BaseParsersTest extends FlatSpec with Matchers with BaseParsers{

    "The BaseParsers" should "parse a newline" in {
        val p = nl
        apply(p, "\n") should equal ("\n")
        an [IllegalArgumentException] should be thrownBy(apply(p, "\r\n"))
        an [IllegalArgumentException] should be thrownBy(apply(p, "  \n"))
    }

    it should "parse whitespace" in {
        val p = ws
        apply(p, " ") should equal (" ")
        apply(p, "\t") should equal ("\t")
        apply(p, "    ") should equal ("    ")
        apply(p, "\t\t") should equal ("\t\t")
        apply(p, "  \t  \t  ") should equal ("  \t  \t  ")
        //we want newlines to be treated diferrently from other ws
        an [IllegalArgumentException] should be thrownBy(apply(p, "\n"))
    }

    it should "be able to look behind" in {
        apply (((elem('a') ~ lookbehind(Set('a')) ~ elem('b'))^^{case a~lb~b=>a+""+b}), "ab") should equal ("ab")
        an [IllegalArgumentException] should be thrownBy {apply (((elem('a') ~ lookbehind(Set('b')) ~ elem('b'))^^{case a~b=>a+""+b}), "ab")}

        apply( (elem('a') ~ not(lookbehind(Set(' ', '\t', '\n'))) ~ '*' ), "a*"  )

    }

    it should "parse chars in ranges" in {
        val p = ranges(SortedMap('A' -> 'Z', '0' -> '9'))
        apply(p, "B") should equal ('B')
        apply(p, "A") should equal ('A')
        apply(p, "Z") should equal ('Z')
        apply(p, "5") should equal ('5')
        apply(p, "0") should equal ('0')
        apply(p, "9") should equal ('9')
        an [IllegalArgumentException] should be thrownBy (apply(p, "a"))
        an [IllegalArgumentException] should be thrownBy (apply(p, "z"))
        an [IllegalArgumentException] should be thrownBy (apply(p, "<"))
    }

}