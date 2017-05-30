package querki.imexport

import fastparse.all._

/**
 * @author jducoeur
 */
trait ParserTests { myself:querki.test.QuerkiTests =>
  
  def checkParse[T](parser:Parser[T], str:String):T = {
    val fullParser = P(parser ~ End)
    fullParser.parse(str).fold({ (parser, index, extra) =>
      val start = 
        if (index < 20)
          index
        else
          index - 20
      fail(s"Attempt to parse MySQL failed in $parser at $index:\n...${str.slice(start, index)}[${str.slice(index, index + 20)}]...")
    }, { (content, _) => content })
  }
  
}