package querki.imexport

import fastparse._, NoWhitespace._

/**
 * @author jducoeur
 */
trait ParserTests { myself: querki.test.QuerkiTests =>

  def Wrap[_ : P, T](p: => P[T]): P[T] = P(p ~ End)

  def checkParse[T](
    wrapped: P[_] => P[T],
    str: String
  ): T = {
    parse(str, wrapped(_)).fold(
      { (parser, index, extra) =>
        val start =
          if (index < 20)
            index
          else
            index - 20
        fail(
          s"Attempt to parse MySQL failed in $parser at $index:\n...${str.slice(start, index)}[${str.slice(index, index + 20)}]..."
        )
      },
      { (content, _) => content }
    )
  }

}
