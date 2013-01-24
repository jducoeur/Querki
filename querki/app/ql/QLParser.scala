package ql

import scala.util.parsing.combinator._

import models.system._
import models._
import controllers.RequestContext

case class TypedValue[VT, CT <% Iterable[ElemValue]](v:CT, pt:PType[VT], ct:Collection[CT])

abstract class ContextBase[VT, CT <% Iterable[ElemValue]] {
  def context:TypedValue[VT,CT]
  def state:SpaceState
  def request:RequestContext
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext[VT, CT <% Iterable[ElemValue]](context:TypedValue[VT,CT], request:RequestContext) extends ContextBase[VT,CT] {
  def state = request.state.getOrElse(SystemSpace.State)
}

case object EmptyContext extends ContextBase[String, Option[ElemValue]] {
  def context:TypedValue[String,Option[ElemValue]] = throw new Exception("Can't use the contents of EmptyContext!")
  def state:SpaceState = throw new Exception("Can't use the space of EmptyContext!")
  def request:RequestContext = throw new Exception("Can't get the request of EmptyContext!")
}

sealed abstract class QLTextPart
case class UnQLText(text:String) extends QLTextPart
case class QLName(name:String)
case class QLPhrase(ops:Seq[QLName])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class ParsedQLText(parts:Seq[QLTextPart])

class QLParser[OVT, OCT <% Iterable[ElemValue]](input:QLText, initialContext:ContextBase[_,_]) extends RegexParsers {
  
  type CB = ContextBase[_,_]
  
  val name = """[a-zA-Z][\w- ]*""".r
  val unQLTextRegex = """([^\[]|\[(?!\[))+""".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = name ^^ { n => QLPhrase(Seq(QLName(n))) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\n") ^^ { QLExp(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]") ^^ { ParsedQLText(_) }
  
  // TODO: this is wrong. The Stage should produce another Context.
  private def processStage(name:QLName, context:ContextBase[_,_]):ContextBase[_,_] = {
    val thing = context.state.anythingByName(name.name)
    val tv = thing match {
      case Some(t) => TypedValue(Some(ElemValue(t.id)), LinkType, ExactlyOne) //LinkType.render(context)(ElemValue(t.id)).internal
      case None => TypedValue(Some(ElemValue("[UNKNOWN NAME: " + name.name + "]")), PlainTextType, ExactlyOne)
    }
    QLContext(tv, context.request)
  }
  
  private def processPhrase(ops:Seq[QLName], startContext:ContextBase[_,_]):ContextBase[_,_] = {
    (startContext /: ops) { (context, stage) => processStage(stage, context) }
  }
  
  private def processPhrases(phrases:Seq[QLPhrase], context:CB):Seq[String] = {
    phrases map (phrase => processPhrase(phrase.ops, context))
  }
  
  private def processParseTree(parseTree:ParsedQLText, context:CB):Wikitext = {
    val strs = parseTree.parts flatMap { 
      _ match {
        case UnQLText(t) => Seq(t)
        case QLExp(phrases) => processPhrases(phrases, context)
      }
    }
    Wikitext(strs.mkString)
  }
  
  def process:Wikitext = {
    val parseResult = parseAll(qlText, input.text)
    parseResult match {
      case Success(result, _) => processParseTree(result, initialContext)
      case Failure(msg, next) => Wikitext("Couldn't parse qlText: " + msg)
      // TODO: we should probably do something more serious in case of Error:
      case Error(msg, next) => Wikitext("ERROR: Couldn't parse qlText: " + msg)
    }
  }
}