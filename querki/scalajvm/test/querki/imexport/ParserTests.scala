package querki.imexport

import fastparse.all._
import Result._

/**
 * @author jducoeur
 */
trait ParserTests { myself:querki.test.QuerkiTests =>
  
  def checkParse[T](parser:Parser[T], str:String) = {
    val fullParser = P(parser ~ End)
    val result = fullParser.parse(str)
    // Why not a match here? Because Scala produces a spurious warning about not being
    // able to deal with the outer type at runtime. Don't know why -- the code works as
    // intended -- but the warning is evil. So we'll fall back to a crude asInstanceOf
    // instead:
    result match {
      case Success(stmts, _) => result
      case Failure(parser, index) => {
        val start = 
          if (index < 10)
            index
          else
            index - 10
        throw new Exception(s"Attempt to parse MySQL failed in $parser at $index:\n...${str.slice(start, index)}[${str.slice(index, index + 20)}]...")
      }
    }
  }
  
}