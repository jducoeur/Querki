package ql

import scala.util.parsing.combinator._

import models.system._
import models._
import controllers.RequestContext

// TODO: we're about ready to obviate away ct here, since it is contained in v.
// Maybe we can do the same for pt?
case class TypedValue(v:PropValue, pt:PType[_]) {
  def ct:Collection = v.coll
  
  def render(context:ContextBase):Wikitext = ct.render(context)(v, pt) 
}

abstract class ContextBase {
  def value:TypedValue
  def state:SpaceState
  def request:RequestContext
}

/**
 * Represents the incoming "context" of a parsed QLText.
 */
case class QLContext(value:TypedValue, request:RequestContext) extends ContextBase {
  def state = request.state.getOrElse(SystemSpace.State)
}

/**
 * This should only be used in cases where we really don't have a context -- generally,
 * displaying outside of a proper page display in a Space. It is unlikely to work for
 * any sophisticated properties, but can be used for PlainText and the like.
 */
case object EmptyContext extends ContextBase {
  def value:TypedValue = throw new Exception("Can't use the contents of EmptyContext!")
  def state:SpaceState = throw new Exception("Can't use the space of EmptyContext!")
  def request:RequestContext = throw new Exception("Can't get the request of EmptyContext!")
}

sealed abstract class QLTextPart
case class UnQLText(text:String) extends QLTextPart
case class QLName(name:String)
case class QLPhrase(ops:Seq[QLName])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class ParsedQLText(parts:Seq[QLTextPart])

class QLParser(input:QLText, initialContext:ContextBase) extends RegexParsers {
  
  val name = """[a-zA-Z][\w- ]*""".r
  val unQLTextRegex = """([^\[]|\[(?!\[))+""".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = name ^^ { n => QLPhrase(Seq(QLName(n))) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\n") ^^ { QLExp(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]") ^^ { ParsedQLText(_) }
  
  // TODO: this is wrong. The Stage should produce another Context.
  private def processStage(name:QLName, context:ContextBase):ContextBase = {
    val thing = context.state.anythingByName(name.name)
    val tv = thing match {
      // TODO: this should call Thing.applyQL().
      // TODO: the following illustrate how broken PropValue and ElemValue are. This line should be
      // TypedValue(ExactlyOne(LinkType(t.id)))
      case Some(t) => TypedValue(ExactlyOne(LinkType(t.id)), LinkType)
      case None => TypedValue(ExactlyOne(PlainTextType("[UNKNOWN NAME: " + name.name + "]")), PlainTextType)
    }
    QLContext(tv, context.request)
  }
  
  private def processPhrase(ops:Seq[QLName], startContext:ContextBase):ContextBase = {
    (startContext /: ops) { (context, stage) => processStage(stage, context) }
  }
  
  private def processPhrases(phrases:Seq[QLPhrase], context:ContextBase):Seq[ContextBase] = {
    phrases map (phrase => processPhrase(phrase.ops, context))
  }

  private def contextsToWikitext(contexts:Seq[ContextBase]):Wikitext = {
    (Wikitext("") /: contexts) { (soFar, context) => soFar + context.value.render(context) }
  }
  
  private def processParseTree(parseTree:ParsedQLText, context:ContextBase):Wikitext = {
    (Wikitext("") /: parseTree.parts) { (soFar, nextPart) =>
      soFar + (nextPart match {
        case UnQLText(t) => Wikitext(t)
        case QLExp(phrases) => contextsToWikitext(processPhrases(phrases, context))
      })
    }
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