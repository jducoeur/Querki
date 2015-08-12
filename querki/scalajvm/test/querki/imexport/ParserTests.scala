package querki.imexport

import fastparse.all._
// Note that we have to import Result directly from its package, not through all,
// due to this Issue:
//   https://github.com/lihaoyi/fastparse/issues/34
import fastparse.core.Result._

/**
 * @author jducoeur
 */
trait ParserTests { myself:querki.test.QuerkiTests =>
  
  def checkParse[T](parser:Parser[T], str:String) = {
    val fullParser = P(parser ~ End)
    val result = fullParser.parse(str)
    result match {
      case Success(stmts, _) => result
      case Failure(parser, index) => {
        val start = 
          if (index < 20)
            index
          else
            index - 20
        fail(s"Attempt to parse MySQL failed in $parser at $index:\n...${str.slice(start, index)}[${str.slice(index, index + 20)}]...")
      }
    }
  }
  
}