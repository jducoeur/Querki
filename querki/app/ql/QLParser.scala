package ql

import scala.util.parsing.combinator._

import models.system._
import models._

case class TypedValue[VT, CT <% Iterable[ElemValue]](v:CT, pt:PType[VT], ct:Collection[CT])

abstract class ContextBase[VT, CT <% Iterable[ElemValue]] {
  def context:TypedValue[VT,CT]
  def state:SpaceState
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext[VT, CT <% Iterable[ElemValue]](context:TypedValue[VT,CT], state:SpaceState) extends ContextBase[VT,CT]

case object EmptyContext extends ContextBase[String, Option[ElemValue]] {
  def context:TypedValue[String,Option[ElemValue]] = throw new Exception("Can't use the contents of EmptyContext!")
  def state:SpaceState = throw new Exception("Can't use the space of EmptyContext!")
}

sealed abstract class QLTextPart
case class UnQLText(text:String) extends QLTextPart
case class QLName(name:String)
case class QLPhrase(ops:Seq[QLName])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class ParsedQLText(parts:Seq[QLTextPart])

class QLParser[OVT, OCT <% Iterable[ElemValue]](input:QLText, context:ContextBase[OVT,OCT]) extends RegexParsers {
  val name = """[a-zA-Z][\w- ]*""".r
  val unQLTextRegex = """([^\[]|\[(?!\[))+""".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = name ^^ { n => QLPhrase(Seq(QLName(n))) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\n") ^^ { QLExp(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]") ^^ { ParsedQLText(_) }
  
  // TODO: this is wrong. The Stage should produce another Context.
  private def processStage(name:QLName):String = {
    val thing = context.state.anythingByName(name.name)
    thing match {
      case Some(t) => LinkType.render(context)(ElemValue(t.id)).internal
      case None => "[UNKNOWN NAME: " + name.name + "]"
    }
  }
  
  private def processPhrase(ops:Seq[QLName]):String = {
    val names = ops map processStage
    names.mkString
  }
  
  private def processPhrases(phrases:Seq[QLPhrase]):Seq[String] = {
    phrases map (phrase => processPhrase(phrase.ops))
  }
  
  private def processParseTree(parseTree:ParsedQLText):Wikitext = {
    val strs = parseTree.parts flatMap { 
      _ match {
        case UnQLText(t) => Seq(t)
        case QLExp(phrases) => processPhrases(phrases)
      }
    }
    Wikitext(strs.mkString)
  }
  
  def process:Wikitext = {
    val parseResult = parseAll(qlText, input.text)
    parseResult match {
      case Success(result, _) => processParseTree(result)
      case Failure(msg, next) => Wikitext("Couldn't parse qlText: " + msg)
      // TODO: we should probably do something more serious in case of Error:
      case Error(msg, next) => Wikitext("ERROR: Couldn't parse qlText: " + msg)
    }
  }
}