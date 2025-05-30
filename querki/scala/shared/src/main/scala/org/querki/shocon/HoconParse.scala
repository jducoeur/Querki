package org.querki.shocon

import fastparse._
import NoWhitespace._

/**
 * Quick-and-dirty rudimentary parser for parts of the HOCON syntax.
 *
 * @author jducoeur
 */
object HoconParse {

  // Explicit whitespace handling
  // (Do we need this? Is modern fastparse's MultiLineWhitespace good enough?)
  def wOptP[_ : P]: P[Unit] = P(CharsWhile(_.isWhitespace, 0))

  // The separator between key-value pairs. Note that commas are optional in HOCON
  def kvSepP[_ : P]: P[Unit] = P(wOptP ~ ("," ~ wOptP).?)

  // A single "name", following JS syntax
  def nameFirstP[_ : P]: P[String] = P((CharPred(_.isLetter) | "_" | "$").!)
  def nameRestP[_ : P]: P[String] = P((CharsWhile(c => c.isLetterOrDigit | c == '_' | c == '$')).!)
  // TODO: if the quotes are present, they must be balanced:
  def nameP[_ : P]: P[String] = P("\"".? ~ (nameFirstP ~ nameRestP).! ~ "\"".?)

  // A path to a final element. This may be one or more levels deep:
  def pathP[_ : P]: P[Seq[String]] = P(nameP.rep(min = 1, sep = "." ~/ Pass))

  // Triple-quoted strings can contain anything, including newlines and ordinary quotes:
  def tripleQuotedP[_ : P]: P[HCValue] = P("\"\"\"" ~ (!"\"\"\"" ~ AnyChar).rep.! ~ "\"\"\"").map(SimpleValue(_))

  // Conventional quoted strings.
  // TODO: interpret \r, \n, etc, correctly.
  // TODO: reject newlines and other control chars inside single-quoted strings.
  def quotedP[_ : P]: P[HCValue] = P("\"" ~ (!"\"" ~ AnyChar).rep.! ~ "\"").map(SimpleValue(_))

  // A sub-object
  def objP[_ : P]: P[HCValue] = P("{" ~ objectGutsP ~ "}")

  def vP[_ : P]: P[HCValue] = P(tripleQuotedP | quotedP | objP)

  // A single key-value pair, but it might be a nested value
  // TODO: according to the HOCON standard, you can omit the "=" in the case of object values
  def kv[_ : P]: P[ObjectValue] = P(pathP ~ wOptP ~ (":" | "=") ~ wOptP ~ vP).map { case (path, v) =>
    val inner = ObjectValue(Map(path.last -> v))
    if (path.length == 1)
      inner
    else
      path.dropRight(1).foldRight(inner) { (node, current) =>
        ObjectValue(Map(node -> current))
      }
  }

  // The actual content of an object -- a bunch of key-value pairs
  def objectGutsP[_ : P]: P[ObjectValue] = P(wOptP ~ kv.rep(sep = kvSepP) ~ wOptP).map { kvs =>
    val allvs = kvs.map(_.vs).reduce(_ ++ _)
    ObjectValue(allvs)
  }

  // The top level -- note that the braces are optional
  // TODO: if the braces are present, they should be balanced
  def topP[_ : P]: P[ObjectValue] = P("{".? ~ wOptP ~ objectGutsP ~ wOptP ~ "}".?)

  /**
   * The main parser. Note that, at the top level, it returns a single "object" value, for the
   * anonymous "root" object. Top-level keys come under that.
   */
  def apply(text: String): ObjectValue = {
    parse(text, topP(_)) match {
      case Parsed.Success(ov, _) => ov
      // TODO: add better failure reporting.
      case Parsed.Failure(parser, index, extra) => {
        throw new Exception(s"Failed to parse in $parser at $index with $extra")
      }
    }
  }
}
