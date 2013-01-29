package ql

import play.api.Logger

import scala.util.parsing.combinator._

import models.system._
import models._
import controllers.RequestContext

/**
 * This is a fake PType, which exists so that we can persist embedded Texts in the pipeline.
 */
object ParsedTextType extends SystemType[Wikitext](OIDs.IllegalOID, () => Thing.emptyProps) with SimplePTypeBuilder[Wikitext]
{
  def doDeserialize(v:String) = throw new Exception("Can't deserialize ParsedText!")
  def doSerialize(v:Wikitext) = throw new Exception("Can't serialize ParsedText!")
  def doRender(context:ContextBase)(v:Wikitext) = v
    
  val doDefault = Wikitext("")
  def wrap(raw:String):valType = Wikitext(raw)
}

// TODO: we've gotten rid of the explicit ct parameter, since it is contained in v.
// Maybe we can do the same for pt?
case class TypedValue(v:PropValue, pt:PType[_]) {
  def ct:Collection = v.coll
  
  def render(context:ContextBase):Wikitext = ct.render(context)(v, pt) 
}
object ErrorValue {
  def apply(msg:String) = TypedValue(ExactlyOne(PlainTextType(msg)), PlainTextType)
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
sealed abstract class QLStage
case class QLName(name:String) extends QLStage
case class QLTextStage(contents:ParsedQLText) extends QLStage
case class QLPhrase(ops:Seq[QLStage])
case class QLExp(phrases:Seq[QLPhrase]) extends QLTextPart
case class ParsedQLText(parts:Seq[QLTextPart])

class QLParser(val input:QLText, initialContext:ContextBase) extends RegexParsers {
  
  val name = """[a-zA-Z][\w- ]*[\w]""".r
  val unQLTextRegex = """([^\[\"]|\[(?!\[)|\"(?!\"))+""".r
  // We don't want the RegexParser removing whitespace on our behalf. Note that we need to be
  // very careful about whitespace!
  override val whiteSpace = "".r
  
  def unQLText:Parser[UnQLText] = unQLTextRegex ^^ { UnQLText(_) }
  def qlName:Parser[QLName] = name ^^ { n => QLName(n) }
  def qlTextStage:Parser[QLTextStage] = "\"\"" ~> qlText <~ "\"\"" ^^ { QLTextStage(_) }
  def qlStage:Parser[QLStage] = qlName | qlTextStage
  // TODO: phrase is going to get a *lot* more complex with time:
  def qlPhrase:Parser[QLPhrase] = rep1sep(qlStage, "\\s*->\\s*".r) ^^ { QLPhrase(_) }
  def qlExp:Parser[QLExp] = rep1sep(qlPhrase, "\n") ^^ { QLExp(_) }
  def qlText:Parser[ParsedQLText] = rep(unQLText | "[[" ~> qlExp <~ "]]") ^^ { ParsedQLText(_) }
  
  /**
   * Note that the output here is nominally a new Context, but the underlying type is
   * ParsedTextType. So far, you can't *do* anything with that afterwards other than
   * render it, which just returns the already-computed Wikitext.
   */
  private def processTextStage(text:QLTextStage, context:ContextBase):ContextBase = {
    val ct = context.value.ct
    val pt = context.value.pt
    // For each element of the incoming context, recurse in and process the embedded Text
    // in that context.
    val transformed = context.value.v.cv map { elem =>
      val elemContext = QLContext(TypedValue(ExactlyOne(elem), pt), context.request)
      ParsedTextType(processParseTree(text.contents, elemContext))
    }
    // TBD: the asInstanceOf here is surprising -- I would have expected transformed to come out
    // as the right type simply by type signature. Can we get rid of it?
    QLContext(TypedValue(ct.makePropValue(transformed.asInstanceOf[ct.implType]), ParsedTextType), context.request)
  }
  
  private def processName(name:QLName, context:ContextBase):ContextBase = {
    val thing = context.state.anythingByName(name.name)
    val tv = thing match {
      case Some(t) => t.qlApply(context)
      case None => ErrorValue("[UNKNOWN NAME: " + name.name + "]")
    }
    QLContext(tv, context.request)
  }
  
  private def processStage(stage:QLStage, context:ContextBase):ContextBase = {
    stage match {
      case name:QLName => processName(name, context)
      case subText:QLTextStage => processTextStage(subText, context)
    }
  }
  
  private def processPhrase(ops:Seq[QLStage], startContext:ContextBase):ContextBase = {
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
  
  def parse = parseAll(qlText, input.text)
  
  def process:Wikitext = {
    val parseResult = parse
    parseResult match {
      case Success(result, _) => processParseTree(result, initialContext)
      case Failure(msg, next) => { Logger.warn("Couldn't parse qlText: " + msg + " at " + next.pos); Wikitext("Couldn't parse qlText: " + msg) }
      // TODO: we should probably do something more serious in case of Error:
      case Error(msg, next) => { Logger.error("Couldn't parse qlText: " + msg); Wikitext("ERROR: Couldn't parse qlText: " + msg) }
    }
  }
}